{-# OPTIONS -Wall -Werror #-}

{- |
Module      :  $Header$
Description :  Tests for Symbion graphs and graph predicates.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Brett Letner
Stability   :  provisional
Portability :  portable

Tests for the Lobster/Symbion.hs module

-}

module Symbion where

import Lobster.Symbion

--------------------------------------------------------------------------------
-- Tests.
--------------------------------------------------------------------------------

checks :: IO ()
checks = runTests $ ta ++ tb ++ tc ++ td

runTests :: Eq a => [GrPred a] -> IO ()
runTests ts =  mapM_ runTest $ zip [1 .. ] ts

runTest :: Eq a => (Int, GrPred a) -> IO ()
runTest (i,p) = do
  putStr $ "symbion test #" ++ show i ++ " "
  case evalGrPred p of
    Ok -> putStrLn "passed."
    Err e -> error $ "failed:" ++ ppErr e

ga :: LGraph Char
ga = mkLGraph show [] [('a','a')]

ta :: [GrPred Char]
ta =
 [ NoPathP 'a' 'a' ga
 , OrP (NoPathP 'a' 'a' ga) (IsPathP 'a' 'a' ga)
 , OrP (IsPathP 'a' 'a' ga) (NoPathP 'a' 'a' ga)
 ]

gb :: LGraph Char
gb = mkLGraph show [] [('a','b')]

tb :: [GrPred Char]
tb =
 [ IsPathP 'a' 'b' gb
 , NoPathP 'b' 'a' gb
 ]

gc :: LGraph Char
gc = mkLGraph show [] [('a','b'),('b','c'),('c','a')]

tc :: [GrPred Char]
tc =
  [ IsPathP 'b' 'a' gc
  ]

gd :: LGraph Char
gd = mkLGraph show [] [('a','b'),('b','c'),('c','b')]

td :: [GrPred Char]
td =
  [ IsPathP 'a' 'c' gd
  , NoPathP 'b' 'a' gd
  ]
