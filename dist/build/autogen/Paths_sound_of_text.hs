module Paths_sound_of_text (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nick/.cabal/bin"
libdir     = "/home/nick/.cabal/lib/x86_64-linux-ghc-7.8.3/sound-of-text-0.1"
datadir    = "/home/nick/.cabal/share/x86_64-linux-ghc-7.8.3/sound-of-text-0.1"
libexecdir = "/home/nick/.cabal/libexec"
sysconfdir = "/home/nick/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sound_of_text_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sound_of_text_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sound_of_text_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sound_of_text_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sound_of_text_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
