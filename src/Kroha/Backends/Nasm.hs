module Kroha.Backends.Nasm where

import Data.Tree
import Data.List (groupBy, intercalate)
import Control.Monad.Fix (fix)
import Control.Monad (join)
import Data.List.Extra (groupSort)

import Kroha.Ast
import Kroha.Instructions hiding (target)

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

nasmBodyWrap body = body

makeFix :: Tree [Instruction] -> [String]
makeFix (Node i c) = join . fmap asmFix $ i
    where asmFix (Body _ i) = fmap ((++) indent) . bodyWrap $ makeFix (c !! i)
          asmFix i          = asm i
          (asm, indent, bodyWrap) = (nasm16I, "  ", id)

nasmSection :: Section -> [[String]] -> String
nasmSection section declarations = header <> body <> "\n\n"
    where header = "section ." ++ section ++ "\n"
          body   = intercalate "\n" . fmap (intercalate "\n") $ declarations

nasmType :: TypeName -> String
nasmType (TypeName "int8" ) = "db"
nasmType (TypeName "int16") = "dw"

nasmDeclaration :: Tree [Instruction] -> Declaration -> [String]
nasmDeclaration t (Frame l _)                               = [l ++ ":"] ++ makeFix t ++ ["leave", "ret"]
nasmDeclaration _ (GlobalVariable   n t (IntegerLiteral l)) = [n ++ ": " ++ nasmType t ++ " " ++ show l]
nasmDeclaration _ (ConstantVariable n t (IntegerLiteral l)) = [n ++ ": " ++ nasmType t ++ " " ++ show l]
nasmDeclaration _ (ManualFrame l c)                         = [l ++ ":", c]
nasmDeclaration _ (ManualVariable v _ c)                    = [v ++ ": " ++ c]


runNasm :: [(Section, Declaration, Tree [Instruction])] -> String
runNasm = join . map mapper
    where mapper (s, d, t) = nasmSection s [nasmDeclaration t d]

