module Policy where

import System.FilePath((<.>))
import qualified System.IO

import Lobster.Monad
--import qualified Lobster.Abs as Abs
import qualified Lobster.Lex as Lex
import qualified Lobster.Par as Par
import qualified Lobster.ErrM as ErrM
--import qualified SCD.Lobster.Domain as Domain
import qualified Lobster.Policy as P
import Lobster.Policy(
  Domain,
  Policy)

parsePolicyFile :: FilePath -> IO (Maybe Policy)
parsePolicyFile filename =
    do chars <- readFile filename
       let toks = Lex.tokens chars
       case Par.pPolicy toks of
         ErrM.Bad e ->
             do putStr ("ERROR: unable to parse " ++ filename ++ ":\n" ++
                        e ++ "\n")
                return Nothing
         ErrM.Ok policy ->
             do putStr ("SUCCESS: parsed " ++ filename ++ "\n")
                return (Just policy)

interpretPolicy :: FilePath -> Policy -> IO (Maybe ([String],Domain))
interpretPolicy filename policy =
    case runP (P.toDomain policy) of
      Left e ->
          do putStr ("ERROR: couldn't interpret " ++ filename ++ ":\n" ++
                     e ++ "\n")
             return Nothing
      Right (eexs,domain) ->
          do sequence_ [ putStrLn x | Right x <- eexs ]
             putStr ("SUCCESS: interpreted " ++ filename ++ "\n")
             System.IO.writeFile
               (filename <.> "lobster")
               (P.prettyPrintDomain domain)
             return (Just ([ e | Left e <- eexs ],domain))

flattenDomain :: FilePath -> Domain -> IO (Maybe Domain)
flattenDomain filename domain =
    case runP (P.flattenDomain domain) of
      Left e ->
          do putStr ("ERROR: couldn't flatten " ++ filename ++ ":\n" ++
                     e ++ "\n")
             return Nothing
      Right domain' ->
          do putStr ("SUCCESS: flattened " ++ filename ++ "\n")
             System.IO.writeFile
               (filename <.> "flatten")
               (P.prettyPrintDomain domain')
             return (Just domain')

testPolicy :: FilePath -> IO Bool
testPolicy testPolicyFilename =
    do mpolicy <- parsePolicyFile testPolicyFilename
       case mpolicy of
         Nothing -> return False
         Just policy ->
             do mx <- interpretPolicy testPolicyFilename policy
                case mx of
                  Just ([],domain) ->
                      do mdomain <- flattenDomain testPolicyFilename domain
                         case mdomain of
                           Nothing -> return False
                           Just _ -> return True
                  _ -> return False

checkPolicy :: (Bool, FilePath) -> IO ()
checkPolicy (shouldFail, testPolicyFilename) =
    do succeeded <- testPolicy testPolicyFilename
       if succeeded /= shouldFail then return ()
          else error $ "ERROR: should have " ++
                       (if shouldFail then "failed" else "succeeded") ++
                       " on " ++ testPolicyFilename

checks :: [(Bool,FilePath)] -> IO ()
checks xs =
    do putStrLn "\nBegin tests of the Lobster compilation"
       mapM_ checkPolicy xs
       putStrLn "End tests of the Lobster compilation"
