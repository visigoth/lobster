import System.Directory ( getCurrentDirectory, setCurrentDirectory
                        , getDirectoryContents, copyFile, createDirectoryIfMissing)
import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit, rawSystemStdout, info, warn, debug)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), InstallDirs(..), absoluteInstallDirs)

main = defaultMainWithHooks simpleUserHooks
    { preConf = \a b -> make a b >> preConf simpleUserHooks a b
    , preClean = \a b -> makeClean a b >> preClean simpleUserHooks a b
    }

-- | The location of the native source, relative to the project root:
nativeCodePath :: FilePath
nativeCodePath = "checkpolicy"

nativeBuildDir :: FilePath -> FilePath
nativeBuildDir cabalDir = cabalDir++"/dist/"++nativeCodePath

make :: Args -> ConfigFlags -> IO ()
make _ flags = do
  let verbosity = (fromFlag $ configVerbosity flags)
  cabalDir <- getCurrentDirectory
  setCurrentDirectory nativeCodePath
  rawSystemLog verbosity "make" []
  setCurrentDirectory cabalDir

makeClean :: Args -> CleanFlags -> IO ()
makeClean _ flags = do
  let verbosity = (fromFlag $ cleanVerbosity flags)
  origDir <- getCurrentDirectory
  setCurrentDirectory nativeCodePath
  rawSystemLog verbosity "make" ["clean"]
  setCurrentDirectory origDir


-- | Execute a system command, logging the output at `info` level.
rawSystemLog :: Verbosity -> String -> [String] -> IO ()
rawSystemLog v cmd args = rawSystemStdout v cmd args >>= info v