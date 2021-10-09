module Kroha.Errors where

import Kroha.Ast
import Data.Bifunctor (bimap, first)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (intercalate)

type Result a = Either Error a

data Error 
    = JoinedError [Error]             {-  -}
    | ParserError String              {-  -}
    | TypeCastError TypeName TypeName NodeId
    | UnknownType TypeName            NodeId
    | UnknownRegister RegisterName    NodeId
    | VariableNotFound VariableName   NodeId
    | LabelNotFound Label             NodeId
    | BackendError String             {-  -}


instance Show Error where
    show (JoinedError errors)     = intercalate "\n" $ fmap show errors
    show (ParserError message)    = "[Parser error]:\n" ++ (unlines . fmap ((++) "\t") . lines) message
    show (TypeCastError t1 t2 _)  = "[Type error]: "    ++ "Can't cast from " ++ show t1 ++ " to " ++ show t2
    show (UnknownType t _)        = "[Type error]: "    ++ "Unknown type " ++ show t
    show (UnknownRegister reg _)  = "[Type error]: "    ++ "Unknown register name " ++ show reg
    show (VariableNotFound var _) = "[Scope error]: "   ++ "Variable " ++ var   ++ " not found in the scope"
    show (LabelNotFound label _)  = "[Scope error]: "   ++ "Label "    ++ label ++ " not found in the scope"
    show (BackendError message)   = "[Asm error]: \n"   ++ (unlines . fmap ((++) "\t") . lines) message

firstE :: (a -> Error) -> Either a b -> Either Error b
firstE = first

partitionErrors :: [Either a b] -> Either [a] [b]
partitionErrors e = let (a, b) = partitionEithers e in if (null a) then Right b else Left a

sequenceErrors :: (Foldable f, Functor f) => ([a] -> c) -> f (Either a b) -> Either c (f b)
sequenceErrors f e = bimap f (const g) $ partitionErrors (toList e)
    where g = fmap (\(Right x) -> x) e
