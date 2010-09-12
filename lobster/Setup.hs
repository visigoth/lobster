import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils (rawSystemExit)

import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          removeFile )
import System.Cmd       ( rawSystem )


main = defaultMainWithHooks simpleUserHooks
       { preBuild = \a b -> bnfc a b >> preBuild simpleUserHooks a b }

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
  putStr "Running bnfc build"
  let verbosity = (fromFlag $ buildVerbosity flags)
  mapM_ removeFile bnfcFiles
  -- cd src && bnfc -d Lobster/Lobster.cf
  orig <- getCurrentDirectory
  setCurrentDirectory "src"
  rawSystemExit verbosity "bnfc" ["-d", "Lobster/Lobster.cf"]
  setCurrentDirectory orig