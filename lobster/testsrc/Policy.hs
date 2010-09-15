module Policy where

import System.FilePath((<.>))
import Directory(getDirectoryContents)
import System.FilePath((</>))

import qualified System.IO
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Lobster.Monad
import Lobster.Policy ( Domain, Policy )

import qualified Lobster.Lex as Lex
import qualified Lobster.Par as Par
import qualified Lobster.ErrM as ErrM
import qualified Lobster.Policy as P

import Test.Framework.Providers.HUnit
import Test.Framework.Providers.API ( Test )
import Test.HUnit hiding ( Test )

testCases :: [(Bool, FilePath)] -> [Test]
testCases lobsterPolicies = map buildCase $ zip [1 .. ] lobsterPolicies

buildCase (i, params) = do
  testCase ("Policy test #"++show i) $ checkPolicy params

checkPolicy :: (Bool, FilePath) -> IO ()
checkPolicy (shouldFail, testPolicyFilename) =
    do succeeded <- testPolicy testPolicyFilename
       if succeeded /= shouldFail then return ()
          else error $ "ERROR: should have " ++
                       (if shouldFail then "failed" else "succeeded") ++
                       " on " ++ testPolicyFilename

testappDirectory :: String
testappDirectory = "../SELinux/testapp"

lobsterExamplePoliciesDirectory :: String
lobsterExamplePoliciesDirectory = "test/examples"

testappPolicy :: String
testappPolicy = testappDirectory </> "testapp.lsr"

getLobsterExamplePolicies :: IO [(Bool,FilePath)]
getLobsterExamplePolicies = do
    fns <- getDirectoryContents lobsterExamplePoliciesDirectory
    return (map rejoin $ List.sort $ Maybe.mapMaybe split fns)

  where
    split :: String -> Maybe (String,Int,String,String)
    split f =
        let (v,f') = List.span Char.isAlpha f in
        let (n,f'') = List.span Char.isDigit f' in
        if f'' == ".lsr" then Just (v, length n, n, f) else Nothing

    rejoin :: (String,Int,String,String) -> (Bool,String)
    rejoin (v,_,_,f) =
        let b = if v == "example" then False
                else if v == "error" then True
                else error $ "bad test file prefix: " ++ v in
        (b, lobsterExamplePoliciesDirectory </> f)

getLobsterPolicies :: IO [(Bool,FilePath)]
getLobsterPolicies = do
    fns <- getLobsterExamplePolicies
    return $ fns ++ [(False,testappPolicy)]

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

