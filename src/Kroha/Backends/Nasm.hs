module Kroha.Backends.Nasm (nasm) where

import Data.Tree
import Data.Graph (buildG)
import Data.List (groupBy, intercalate)
import Control.Monad.Fix (fix)
import Control.Monad (join)
import Data.List.Extra (groupSort)
import Data.Bifunctor (first)

import Kroha.Ast
import Kroha.Backends.Common
import Kroha.Types
import Kroha.Instructions (Instruction(..), LabelTarget(..), Target(..), Section)
import Kroha.Errors

bytes :: Int -> Int
bytes x = ceiling ((toEnum x) / 8)

label (CommonLabel l) = l
label (BeginLabel  l) = l ++ "_begin"
label (EndLabel    l) = l ++ "_end"

target :: Target -> String
target (LiteralTarget (IntegerLiteral num)) = show num
target (StackTarget (offset, _))            = "[bp - " ++ show (bytes offset) ++ "]"
target (RegisterTarget reg)                 = reg
target (VariableTarget name)                = '[' : name ++ "]"

jump :: Comparator -> String
jump Equals    = "je"
jump NotEquals = "jne"
jump Less      = "jl"
jump Greater   = "jg"

nasm16I (Body _ i)                   = []
nasm16I (Assembly asm)               = [asm]
nasm16I (Label lbl)                  = [label lbl ++ ":"]
nasm16I (Move l r)                   = ["mov " ++ target l ++ ", " ++ target r]
nasm16I (CallI l args)               = (fmap (((++) "push ") . target) . reverse $ args) ++ ["call " ++ label l, "add sp, " ++ show ((length args) * 2)]
nasm16I (Jump l Nothing)             = ["jmp " ++ label l]
nasm16I (Jump lbl (Just (l, c, r)))  = ["cmp " ++ target l ++ ", " ++ target r, jump c ++ " " ++ label lbl]

nasmSection :: Section -> String -> String
nasmSection section body = header <> body <> "\n\n"
    where header = "section ." ++ section ++ "\n"

nasmType :: TypeName -> String
nasmType (TypeName "int8" ) = "db"
nasmType (TypeName "int16") = "dw"

nasmDeclaration :: Declaration -> [String] -> String
nasmDeclaration (Frame l _)                               body = l ++ ":\n" ++ intercalate "\n" body ++ "\nleave\nret\n"
nasmDeclaration (ManualFrame l _)                         body = l ++ ":\n" ++ intercalate "\n" body ++"\n"
nasmDeclaration (ManualVariable v _ _)                    body = v ++ ": "  ++ intercalate "\n" body ++"\n"
nasmDeclaration (GlobalVariable   n t (IntegerLiteral l)) _    = n ++ ": "  ++ nasmType t ++ " " ++ show l ++"\n"
nasmDeclaration (ConstantVariable n t (IntegerLiteral l)) _    = n ++ ": "  ++ nasmType t ++ " " ++ show l ++"\n"

litType :: Literal -> Result TypeId
litType l@(IntegerLiteral x) | x >= 0   && x < 65536 = Right 2
                             | otherwise             = Left (BackendError (show l ++ " is not in [0; 65536)"))

nasmTypes = TypeConfig 
    { types = (fmap . first) TypeName [("int8", 8), ("int16", 16), ("+literal+", 16)]
    , pointerType = 1
    , registers = zip (join $ fmap (\x -> fmap ((:) x . pure) "lhx") "abcd") (cycle [0, 0, 1])
    , typeCasts = buildG (0, 3) [(0, 2), (1, 2)]
    , literalType = litType }

nasm = Backend 
    { instruction = nasm16I
    , bodyWrap    = id
    , indent      = "  "
    , section     = nasmSection
    , declaration = nasmDeclaration
    , typeConfig  = nasmTypes }
