import Distribution.Simple
import Distribution.Simple.Program.Types
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils ( rawSystemExit, warn, debug, findProgramVersion )
import Distribution.Verbosity ( Verbosity )

import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          removeFile )
import System.Cmd       ( rawSystem )


main = do
  defaultMainWithHooks simpleUserHooks
       { preBuild = \a b -> bnfc a b >> preBuild simpleUserHooks a b
       , hookedPrograms = [bnfcProgram] }

bnfcProgram :: Program
bnfcProgram = (simpleProgram "bnfc") {
  programFindVersion = findProgramVersion "--numeric-version" id
  }

bnfcFiles :: [FilePath]
bnfcFiles = [ "src/Lobster/Abs.hs"
            , "src/Lobster/Doc.tex"
            , "src/Lobster/Doc.txt"
            , "src/Lobster/ErrM.hs"
            , "src/Lobster/Lex.x"
            , "src/Lobster/Par.y"
            , "src/Lobster/Print.hs"
            , "src/Lobster/Skel.hs"
            , "src/Lobster/Test.hs"
            ]

-- translation of this make file rule:
-- $(BNFC_FILES): src/Lobster/Lobster.cf
-- 	rm -f $(BNFC_FILES)
-- 	cd src/Lobster && $(BNFC) -d Lobster.cf
-- 	mv src/Lobster/Lobster/*.* src/Lobster
-- 	rmdir src/Lobster/Lobster

bnfc :: Args -> BuildFlags -> IO ()
bnfc _ flags = do
  let verbosity = (fromFlag $ buildVerbosity flags)
  warn verbosity "Running bnfc build"
  mapM_ (removeFileIfExists verbosity) bnfcFiles
  -- cd src && bnfc -d Lobster/Lobster.cf
  orig <- getCurrentDirectory
  setCurrentDirectory "src"
  rawSystemExit verbosity "bnfc" ["-d", "Lobster/Lobster.cf"]
  setCurrentDirectory orig

removeFileIfExists :: Verbosity -> FilePath -> IO ()
removeFileIfExists v file = removeFile file `catch`
                            \e -> debug v ("Could not remove file "++file++
                                          " due to exception: "++show e)