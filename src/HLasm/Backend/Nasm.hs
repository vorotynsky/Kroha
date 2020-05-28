-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Backend.Nasm (nasm) where

import           HLasm.Ast          (CompareType (..))
import           HLasm.Error
import           HLasm.Frame
import           HLasm.Instructions hiding (program)

bytes :: Int -> Int
bytes x = ceiling ((toEnum x) / 8)

choosePtr :: Int -> String
choosePtr = f . bytes
    where f 1 = "BYTE"
          f 2 = "WORD"
          f 4 = "DWORD"
          f n = error ("undefined data size: " ++ show n)

target :: Target -> String
target (NamedTarget name)           = name
target (Register reg)               = reg
target (FrameVar (offset, size, _)) = choosePtr size ++ " [ebp-" ++ (show $ bytes (offset + size)) ++ "]"
target (ConstantTarget const)       = show const

instr2arg :: String -> Target -> Target -> String
instr2arg n l r = n ++ " " ++ target l ++ ", " ++ target r

size :: StackFrame -> Int
size (Root)           = 0
size (Fluent _)       = 0
size (StackFrame _ x) = bytes $ frameSize x

frame f = ["push ebp", "mov ebp, esp", "sub esp, " ++ (show . size $ f)]

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
instruction (Call lbl args size)        = (fmap push . reverse $ args) ++ ["call " ++ lbl, "add esp, " ++ show (bytes size)]
    where push x = "push " ++ (target x)

join :: String -> [String] -> String
join s []     = ""
join s [x]    = x
join s (x:xs) = x ++ s ++ join s xs

section :: Section -> Result String
section (Text x) = Right .  join "\n" . ((:) "section .text\n") . concat $ fmap instruction x

program :: ObjProgram -> Result String
program (ObjProgram sections) = fmap (join "\n\n") . traverse section $ sections

nasm :: BackEnd
nasm = BackEnd program
