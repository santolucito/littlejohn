module Paths_littlejohn (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/mark/.cabal/bin"
libdir     = "/home/mark/.cabal/lib/littlejohn-0.1/ghc-7.6.3"
datadir    = "/home/mark/.cabal/share/littlejohn-0.1"
libexecdir = "/home/mark/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "littlejohn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "littlejohn_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "littlejohn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "littlejohn_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
