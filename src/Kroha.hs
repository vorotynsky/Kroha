module Kroha (kroha) where

import           Data.Bifunctor        (first)

import           Kroha.Ast             (NodeId, Program, genId, pzip, pzip3)
import           Kroha.Backends.Common (runBackend, typeConfig)
import           Kroha.Backends.Nasm   (nasm)
import           Kroha.Errors          (Error (ParserError), Result)
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

kroha :: String -> Either String String
kroha src = case fmap genId (parse src) of
                         Left (ParserError err) -> Left err
                         Left _                 -> error "Unexpected parser error type"
                         Right prog             -> first show $ compile prog
