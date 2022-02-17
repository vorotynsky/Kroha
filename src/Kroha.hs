module Kroha (kroha) where

import           Data.Bifunctor            (first)
import           Data.Foldable             (toList)
import           Data.HashMap              (fromList, lookup)
import           Control.Monad             (void)

import           Kroha.Backends.Common     (Backend, runBackend, typeConfig)
import           Kroha.Backends.Nasm       (genNasm)
import           Kroha.Backends.Registers  (RegFile)
import           Kroha.Errors              (Result, showErrors)
import           Kroha.Instructions        (instructions)
import           Kroha.Parser.Declarations (parseProgram)
import           Kroha.Scope               (linkProgram)
import           Kroha.Stack               (stack)
import           Kroha.Syntax.Declarations (NodeId, Program, genId, pzip, pzip3, Program(..), Declaration(RegisterDeclaration))
import           Kroha.Types               (resolve, typeCastsTree)


compile :: Backend -> Program NodeId -> Result String
compile backend program = 
      do scopes  <- linkProgram program
         let tc   = typeConfig backend
         casts   <- typeCastsTree tc scopes
         _       <- resolve tc (pzip program casts)
         let stackRanges = stack tc program
         let prepared = instructions (pzip3 stackRanges (fmap snd scopes) program)
         return (runBackend backend prepared)

kroha :: RegFile -> String -> String -> Either String String
kroha regfile name src =
      case parseProgram name src of
           Left err   -> Left err
           Right parsed -> first (showErrors (`Data.HashMap.lookup` rangeTable)) $ compile (genNasm regfile) prog
              where addRegs (Program decls ()) = Program (decls ++ fmap (`RegisterDeclaration` ()) regfile) ()
                    prog = genId . addRegs . void $ parsed
                    rangeTable = fromList $ toList $ pzip prog parsed
