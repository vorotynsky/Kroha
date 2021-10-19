module Kroha (kroha) where

import           Data.Bifunctor        (first)
import           Data.Foldable         (toList)
import           Data.HashMap          (fromList, lookup)

import           Kroha.Syntax             (NodeId, Program, genId, pzip, pzip3)
import           Kroha.Backends.Common (runBackend, typeConfig)
import           Kroha.Backends.Nasm   (nasm)
import           Kroha.Errors          (Result, showErrors)
import           Kroha.Instructions    (instructions)
import           Kroha.Parser          (parse)
import           Kroha.Scope           (linkProgram)
import           Kroha.Stack           (stack)
import           Kroha.Types           (resolve, typeCastsTree)


compile :: Program NodeId -> Result String
compile program = do
                  scopes  <- linkProgram program
                  let tc   = typeConfig nasm
                  casts   <- typeCastsTree tc scopes
                  types   <- resolve tc (pzip program casts)
                  let stackRanges = stack tc program
                  let prepared = instructions (pzip3 stackRanges (fmap snd scopes) program)
                  return (runBackend nasm prepared)

kroha :: String -> String -> Either String String
kroha name src =
      case parse name src of
           Left err   -> Left err
           Right parsed -> first (showErrors (`Data.HashMap.lookup` rangeTable)) $ compile prog
              where prog = genId parsed
                    rangeTable = fromList $ toList $ pzip prog parsed


