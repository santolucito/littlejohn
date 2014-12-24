module Paths_site (
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
libdir     = "/home/mark/.cabal/lib/site-0.1/ghc-7.6.3"
datadir    = "/home/mark/.cabal/share/site-0.1"
libexecdir = "/home/mark/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "site_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "site_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "site_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "site_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
