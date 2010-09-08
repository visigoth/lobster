{-# OPTIONS_GHC -Wall -Werror #-}
{- |
Module      :  $Header$
Description :  Tests the SELinux policy symbol table
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Tests the SELinux policy symbol table.
-}
module SCD.SELinux.Test.Symbol where

import SCD.SELinux.Syntax(Policy)
import SCD.SELinux.Symbol(SymbolTable,build,summarize)
import SCD.SELinux.Test.Parser(parsePolicyFile)
import Prelude hiding (FilePath)

tryBuildSymbolTable :: Policy -> Either String (Policy,SymbolTable)
tryBuildSymbolTable policy = build policy

buildSymbolTable :: String -> Policy -> (Policy,SymbolTable)
buildSymbolTable filename policy =
    case tryBuildSymbolTable policy of
      Left err ->
          error ("ERROR: couldn't build the symbol table for " ++ filename ++
                 ":\n" ++ err)
      Right policy_symbols ->
          policy_symbols

buildSymbolTableTest :: String -> Policy -> IO (Policy,SymbolTable)
buildSymbolTableTest filename policy =
    do (policy',symbols) <- return (buildSymbolTable filename policy)
       putStr ("SUCCESS: built the symbol table for " ++ filename ++
               ":\n" ++ summarize symbols)
       return (policy',symbols)

symbolFileTest :: String -> IO (Policy,SymbolTable)
symbolFileTest filename =
    do policy <- parsePolicyFile filename
       buildSymbolTableTest filename policy

checks :: String -> Policy -> IO (Policy,SymbolTable)
checks filename policy =
    do
      putStrLn "\nBegin tests of the SELinux policy symbol table"
      _ <- symbolFileTest "src/SCD/SELinux/Test/data/example.conf"
      (policy',symbols) <- buildSymbolTableTest filename policy
      putStrLn "End tests of the SELinux policy symbol table"
      return (policy',symbols)
