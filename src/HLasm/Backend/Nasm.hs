-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Backend.Nasm (nasm) where

import           Data.Char

import           HLasm.Ast          (CompareType (..), Type(..))
import           HLasm.Error
import           HLasm.Frame
import           HLasm.Instructions hiding (program)

bytes :: Int -> Int
bytes x = ceiling ((toEnum x) / 8)

data DataType = Byte | Word | Dword
    deriving (Show, Eq)

uname = fmap toUpper . show

dname Byte  = "DB"
dname Word  = "DW"
dname Dword = "DD"

datatype :: Int -> DataType
datatype = f . bytes
    where f 1 = Byte
          f 2 = Word
          f 4 = Dword
          f n = error ("undefined data size: " ++ show n)

toDatatype :: Type -> DataType
toDatatype = let size (Type _ (Just s)) = s in datatype . size

target :: Target -> String
target (NamedTarget name)           = name
target (Register reg)               = reg
target (FrameVar (offset, size, _)) = (uname . datatype) size ++ " [bp-" ++ (show $ bytes (offset + size)) ++ "]"
target (ConstantTarget const)       = show const

instr2arg :: String -> Target -> Target -> String
instr2arg n l r = n ++ " " ++ target l ++ ", " ++ target r

size :: StackFrame -> Int
size (Root)           = 0
size (Fluent _)       = 0
size (StackFrame _ x) = bytes $ frameSize x

frame f = ["push bp", "mov bp, sp", "sub sp, " ++ (show . size $ f)]

instruction :: Instructions -> [String]
instruction (PureAsm str)               = [str]
instruction (BeginFrame f (Just l))     = (l ++ ":"):(frame f)
instruction (EndFrame   f (Just _))     = ["leave", "ret"]
instruction (BeginFrame f Nothing)      = frame f
instruction (EndFrame   f Nothing)      = ["leave"]
instruction (Label l)                   = [l ++ ":"]
instruction (Move l r)                  = [instr2arg "mov" l r]
instruction (Compare l r)               = [instr2arg "cmp" l r]
instruction (Jump lbl Nothing)          = ["jmp " ++ lbl]
instruction (Jump lbl (Just Equals))    = ["je " ++ lbl]
instruction (Jump lbl (Just NotEquals)) = ["jne " ++ lbl]
instruction (Jump lbl (Just Greater))   = ["jg " ++ lbl]
instruction (Jump lbl (Just Less))      = ["jl " ++ lbl]
instruction (Call lbl args size)        = (fmap push . reverse $ args) ++ ["call " ++ lbl, "add sp, " ++ show (bytes size)]
    where push x = "push " ++ (target x)

variable :: Variable -> String
variable (Variable n t v)= n ++ ": " ++ (dname . toDatatype) t ++ " " ++ show v

join :: String -> [String] -> String
join s []     = ""
join s [x]    = x
join s (x:xs) = x ++ s ++ join s xs

sectionHeader header = Right .  join "\n" . ((:) ("section ." ++ header))

section :: Section -> Result String
section (Text x)      = sectionHeader "text\n" . concat $ fmap instruction x
section (Data x)      = sectionHeader "data"          $ fmap variable x
section (Constants x) = sectionHeader "rodata"        $ fmap variable x

program :: ObjProgram -> Result String
program (ObjProgram sections) = fmap (join "\n\n") . traverse section $ sections

nasm :: BackEnd
nasm = BackEnd program
