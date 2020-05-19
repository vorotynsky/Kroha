-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Backend.Nasm (nasm) where

import           HLasm.Ast          (CompareType (..))
import           HLasm.Frame
import           HLasm.Instructions

bytes :: Int -> Int
bytes x = ceiling ((toEnum x) / 8)

choosePtr :: Int -> String
choosePtr = f . bytes
    where f 1 = "BYTE"
          f 2 = "WORD"
          f 4 = "DWORD"
          f _ = error "undefined ptr size"

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
instruction (BeginFrame f Nothing)      = frame f
instruction (EndFrame f)                = ["leave", "ret"]
instruction (Label l)                   = [l ++ ":"]
instruction (Move l r)                  = [instr2arg "mov" l r]
instruction (Compare l r)               = [instr2arg "cmp" l r]
instruction (Jump lbl Nothing)          = ["jmp " ++ lbl]
instruction (Jump lbl (Just Equals))    = ["je " ++ lbl]
instruction (Jump lbl (Just NotEquals)) = ["jne " ++ lbl]
instruction (Jump lbl (Just Greater))   = ["jg " ++ lbl]
instruction (Jump lbl (Just Less))      = ["jl " ++ lbl]
instruction (Call lbl args)             = (fmap push . reverse $ args) ++ ["call " ++ lbl ++ "; HACK: stack doesn't align"] -- TOOD
    where push x = "push " ++ (target x)

join :: String -> [String] -> String
join s []     = ""
join s [x]    = x
join s (x:xs) = x ++ s ++ join s xs

nasm :: BackEnd
nasm = BackEnd (\x -> join "\n" . concat $ fmap instruction x)
