module Paths_pokesim_g03 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/carla/.cabal/bin"
libdir     = "/home/carla/.cabal/lib/pokesim-g03-0.1.0.0/ghc-7.4.2"
datadir    = "/home/carla/.cabal/share/pokesim-g03-0.1.0.0"
libexecdir = "/home/carla/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "pokesim_g03_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pokesim_g03_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "pokesim_g03_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pokesim_g03_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
