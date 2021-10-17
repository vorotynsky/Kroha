{-# LANGUAGE TupleSections #-}
module Kroha.Errors where

import Kroha.Ast
import Data.Bifunctor (bimap, first)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List.Extra
import Data.Maybe (fromMaybe)
import Text.Parsec (SourcePos, sourceName, sourceColumn, sourceLine)

type Result a = Either Error a

data Error
    = JoinedError [Error]             {-  -}
    | TypeCastError TypeName TypeName NodeId
    | UnknownType TypeName            NodeId
    | UnknownRegister RegisterName    NodeId
    | VariableNotFound VariableName   NodeId
    | LabelNotFound Label             NodeId
    | BackendError String             {-  -}
    deriving Eq

getErrorId (JoinedError _)        = -1
getErrorId (TypeCastError _ _ i)  = i
getErrorId (UnknownType _ i)      = i
getErrorId (UnknownRegister _ i)  = i
getErrorId (VariableNotFound _ i) = i
getErrorId (LabelNotFound _ i)    = i
getErrorId (BackendError _)       = -1

instance Ord Error where
    a <= b = getErrorId a <= getErrorId b


firstE :: (a -> Error) -> Either a b -> Either Error b
firstE = first

partitionErrors :: [Either a b] -> Either [a] [b]
partitionErrors e = let (a, b) = partitionEithers e in if (null a) then Right b else Left a

sequenceErrors :: (Foldable f, Functor f) => ([a] -> c) -> f (Either a b) -> Either c (f b)
sequenceErrors f e = bimap f (const g) $ partitionErrors (toList e)
    where g = fmap (\(Right x) -> x) e

toErrorList :: [Error] -> [Error]
toErrorList = concatMap mapper
    where mapper (JoinedError errors) = toErrorList errors
          mapper error                = [error]


sNub :: Eq a => [a] -> [a]
sNub (a:b:tail) | a == b    = sNub (b:tail)
                | otherwise = a:sNub (b:tail)
sNub xs                     = xs

showErrors :: (NodeId -> Maybe (SourcePos, SourcePos)) -> Error -> String
showErrors findRange = intercalate "\n" . fmap (uncurry showError) . process . toErrorList . pure
    where showError r (JoinedError _)          = undefined
          showError r (TypeCastError t1 t2 d)  = r ++ "[Type error]:\t"  ++ "Can't cast from " ++ show t1 ++ " to " ++ show t2
          showError r (UnknownType t d)        = r ++ "[Type error]:\t"  ++ "Unknown type " ++ show t
          showError r (UnknownRegister reg d)  = r ++ "[Type error]:\t"  ++ "Unknown register name " ++ show reg
          showError r (VariableNotFound var d) = r ++ "[Scope error]:\t" ++ "Variable " ++ var   ++ " not found in the scope"
          showError r (LabelNotFound label d)  = r ++ "[Scope error]:\t" ++ "Label "    ++ label ++ " not found in the scope"
          showError r (BackendError message)   = "[Asm error]: \n" ++ (unlines . fmap ((++) "\t") . lines) message
          showRange' (begin, end) = sourceName begin ++ ":" ++ show (sourceLine begin) ++ ":" ++ show (sourceColumn begin) ++ ":\t"
          zipFrom (a, b) = fmap (a, ) b
          process = concatMap (zipFrom . bimap (maybe "" showRange') nub) . groupSort . map (\x -> (findRange $ getErrorId x, x))
