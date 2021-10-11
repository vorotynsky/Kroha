module Kroha.Errors where

import Kroha.Ast
import Data.Bifunctor (bimap, first)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (intercalate, sort)
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
    where mapper (JoinedError errors) = errors
          mapper error                = [error]


showErrors :: (NodeId -> Maybe (SourcePos, SourcePos)) -> Error -> String
showErrors findRange = showError . JoinedError . sort . toErrorList . pure
    where showError (JoinedError errors)     = intercalate "\n\n" $ fmap showError errors
          showError (TypeCastError t1 t2 d)  = showRange d ++ "[Type error]: "  ++ "Can't cast from " ++ show t1 ++ " to " ++ show t2
          showError (UnknownType t d)        = showRange d ++ "[Type error]: "  ++ "Unknown type " ++ show t
          showError (UnknownRegister reg d)  = showRange d ++ "[Type error]: "  ++ "Unknown register name " ++ show reg
          showError (VariableNotFound var d) = showRange d ++ "[Scope error]: " ++ "Variable " ++ var   ++ " not found in the scope"
          showError (LabelNotFound label d)  = showRange d ++ "[Scope error]: " ++ "Label "    ++ label ++ " not found in the scope"
          showError (BackendError message)   = "[Asm error]: \n" ++ (unlines . fmap ((++) "\t") . lines) message
          showRange = maybe "" showRange' . findRange
          showRange' (begin, end) = sourceName begin ++ ":" ++ show (sourceLine begin) ++ ":" ++ show (sourceColumn begin) ++ ": "
