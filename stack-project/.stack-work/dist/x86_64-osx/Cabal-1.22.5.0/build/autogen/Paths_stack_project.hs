module Paths_stack_project (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/edmund/Dropbox/Programming/Haskell/src/stack-project/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/bin"
libdir     = "/Users/edmund/Dropbox/Programming/Haskell/src/stack-project/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/lib/x86_64-osx-ghc-7.10.3/stack-project-0.1.0.0-KkcMvpQySTEJerCRWTTbbY"
datadir    = "/Users/edmund/Dropbox/Programming/Haskell/src/stack-project/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/share/x86_64-osx-ghc-7.10.3/stack-project-0.1.0.0"
libexecdir = "/Users/edmund/Dropbox/Programming/Haskell/src/stack-project/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/libexec"
sysconfdir = "/Users/edmund/Dropbox/Programming/Haskell/src/stack-project/.stack-work/install/x86_64-osx/lts-5.0/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "stack_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "stack_project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "stack_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stack_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "stack_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
