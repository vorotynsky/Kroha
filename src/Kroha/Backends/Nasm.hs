module Kroha.Backends.Nasm (nasm) where

import           Data.Bifunctor        (first)
import           Data.Graph            (buildG)
import           Data.List             (intercalate)
import           Data.Maybe            (fromJust)

import           Kroha.Backends.Common
import           Kroha.Errors
import           Kroha.Instructions    (Instruction (..), LabelTarget (..), Section, Target (..))
import           Kroha.Syntax.Syntax
import           Kroha.Types

bytes :: Int -> Int
bytes x = ceiling (toEnum x / (8 :: Double))

label :: LabelTarget -> Label
label (CommonLabel l) = l
label (BeginLabel  l) = l ++ "_begin"
label (EndLabel    l) = l ++ "_end"

nasmType :: TypeName -> (String, String)
nasmType (TypeName "int8" ) = ("db", "byte")
nasmType (TypeName "int16") = ("dw", "word")
nasmType (PointerType    _) = ("dw", "word")
nasmType (TypeName name   ) = error $ "[Exception]: Unexpected type `" ++ name ++ "` in backend"

size2type :: Int -> TypeName
nasmTypeG , nasmTypeL, untyped :: TypeName -> String
(nasmTypeG, nasmTypeL, untyped) = (fst . nasmType, append " " . snd . nasmType, const "")
    where append x s = s ++ x


target :: (TypeName -> String) -> Target -> String
target tf (LiteralTarget (IntegerLiteral num)) = show num
target tf (StackTarget (offset, s))            = (tf . size2type) s ++ "[bp - " ++ show (bytes offset) ++ "]"
target tf (RegisterTarget reg)                 = reg
target tf (VariableTarget name t)              = tf t ++ "[" ++ name ++ "]"

jump :: Comparator -> String
jump Equals    = "je"
jump NotEquals = "jne"
jump Less      = "jl"
jump Greater   = "jg"

nasm16I :: Instruction -> [String]
nasm16I (Body _ _)                   = []
nasm16I (Assembly asm)               = [asm]
nasm16I (Label lbl)                  = [label lbl ++ ":"]
nasm16I (Move l r)                   = ["mov " ++ target nasmTypeL l ++ ", " ++ target untyped r]
nasm16I (CallI l args)               = (fmap (("push " ++) . target nasmTypeL) . reverse $ args) ++ ["call " ++ label l, "add sp, " ++ show ((length args) * 2)]
nasm16I (Jump l Nothing)             = ["jmp " ++ label l]
nasm16I (Jump lbl (Just (l, c, r)))  = ["cmp " ++ target nasmTypeL l ++ ", " ++ target untyped r, jump c ++ " " ++ label lbl]

nasmSection :: Section -> String -> String
nasmSection section body = header <> body <> "\n\n"
    where header = "section ." ++ section ++ "\n"

nasmDeclaration :: Declaration d -> [String] -> String
nasmDeclaration (Frame l _ _)                               body  = l ++ ":\n" ++ intercalate "\n" body ++ "\nleave\nret"
nasmDeclaration (ManualVariable v _ _ _)                   [body] = v ++ ": "  ++ body ++ "\n"
nasmDeclaration (ManualFrame l _ _)                         body  = l ++ ":\n" ++ intercalate "\n" (fmap ("  " ++) body)
nasmDeclaration (ManualVariable v _ _ _)                    body  = v ++ ":\n" ++ intercalate "\n" (fmap ("  " ++) body)
nasmDeclaration (GlobalVariable   n t (IntegerLiteral l) _) _     = n ++ ": "  ++ nasmTypeG t ++ " " ++ show l
nasmDeclaration (ConstantVariable n t (IntegerLiteral l) _) _     = n ++ ": "  ++ nasmTypeG t ++ " " ++ show l

litType :: Literal -> Result TypeId
litType l@(IntegerLiteral x) | x >= 0   && x < 65536 = Right 2
                             | otherwise             = Left (BackendError (show l ++ " is not in [0; 65536)"))

nasmTypes = TypeConfig
    { types = (fmap . first) TypeName [("int8", 8), ("int16", 16), ("+literal+", 16)]
    , pointerType = 1
    , registers = zip ((\x -> fmap ((:) x . pure) "lhx") =<< "abcd") (cycle [0, 0, 1])
    , typeCasts = buildG (0, 3) [(0, 2), (1, 2)]
    , literalType = litType }

size2type size = fromJust . lookup size . fmap (\(a, b) -> (b, a)) $ types nasmTypes

nasm = Backend
    { instruction = nasm16I
    , bodyWrap    = id
    , indent      = "  "
    , section     = nasmSection
    , declaration = nasmDeclaration
    , typeConfig  = nasmTypes }
