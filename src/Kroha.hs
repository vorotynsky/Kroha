module Kroha (kroha) where

import           Data.Bifunctor            (first)
import           Data.Foldable             (toList)
import           Data.HashMap              (fromList, lookup)

import           Kroha.Backends.Common     (Backend, runBackend, typeConfig)
import           Kroha.Errors              (Result, showErrors)
import           Kroha.Instructions        (instructions)
import           Kroha.Parser.Declarations (parseProgram)
import           Kroha.Scope               (linkProgram)
import           Kroha.Stack               (stack)
import           Kroha.Syntax.Declarations (NodeId, Program, genId, pzip, pzip3)
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

kroha :: Backend -> String -> String -> Either String String
kroha backend name src =
      case parseProgram name src of
           Left err   -> Left err
           Right parsed -> first (showErrors (`Data.HashMap.lookup` rangeTable)) $ compile backend prog
              where prog = genId parsed
                    rangeTable = fromList $ toList $ pzip prog parsed
