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


target :: (?bp :: RegisterName) => (TypeName -> String) -> Target -> String
target tf (LiteralTarget (IntegerLiteral num)) = show num
target tf (StackTarget (offset, s))            = (tf . size2type) s ++ "[bp - " ++ show (bytes offset) ++ "]"
target tf (RegisterTarget reg)                 = reg
target tf (VariableTarget name t)              = tf t ++ "[" ++ name ++ "]"

jump :: Comparator -> String
jump Equals    = "je"
jump NotEquals = "jne"
jump Less      = "jl"
jump Greater   = "jg"

nasmI :: (?sp :: RegisterName, ?bp :: RegisterName) => Instruction -> [String]
nasmI (Body _ _)                   = []
nasmI (Assembly asm)               = [asm]
nasmI (Label lbl)                  = [label lbl ++ ":"]
nasmI (Move l r)                   = ["mov " ++ target nasmTypeL l ++ ", " ++ target untyped r]
nasmI (CallI l args)               = (fmap (("push " ++) . target nasmTypeL) . reverse $ args) ++ ["call " ++ label l, "add " ++ ?sp ++ ", " ++ show ((length args) * 2)]
nasmI (Jump l Nothing)             = ["jmp " ++ label l]
nasmI (Jump lbl (Just (l, c, r)))  = ["cmp " ++ target nasmTypeL l ++ ", " ++ target untyped r, jump c ++ " " ++ label lbl]
nasmI (StackAlloc s)               = ["enter " ++ show (bytes s) ++ ", 0"]

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

regsBase16 = zip ((\x -> fmap ((:) x . pure) "lhx") =<< "abcd") (cycle [0, 0, 1])
regsBase32 = zip ((fmap (\x -> 'e':x:"x")) "abcd") (cycle [3])
regsBase64 = zip ((fmap (\x -> 'r':x:"x")) "abcd") (cycle [4])

regsSpec r = zip ['r':r, 'e':r, r, r++"l"] [4,3,1,0]
regsInfo = concatMap regsSpec ["sp", "bp", "si", "di"]

regsRf r = zip [r, r++"d", r++"w", r++"b"] [4, 3, 1, 0]
regsR = concatMap regsRf $ fmap ((:) 'r' . show) [8..15]

nasmTypes = TypeConfig
    { types = (fmap . first) TypeName [("int8", 8), ("int16", 16), ("+literal+", 16), ("int32", 32), ("int64", 64)]
    , pointerType = 1
    , registers = regsBase16 ++ regsBase32 ++ regsBase64 ++ regsInfo ++ regsR
    , typeCasts = buildG (0, 5) [(0, 2), (1, 2), (3, 2), (4, 2)]
    , literalType = litType }

size2type size = fromJust . lookup size . fmap (\(a, b) -> (b, a)) $ types nasmTypes

nasmInstruction 16 = let ?bp =  "bp" in let ?sp =  "sp" in nasmI
nasmInstruction 32 = let ?bp = "ebp" in let ?sp = "esp" in nasmI
nasmInstruction 64 = let ?bp = "rbp" in let ?sp = "rsp" in nasmI


nasm arch = Backend
    { instruction = nasmInstruction arch
    , bodyWrap    = id
    , indent      = "  "
    , section     = nasmSection
    , declaration = nasmDeclaration
    , typeConfig  = nasmTypes }
