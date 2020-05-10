{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_companion (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dragemil/.cabal/bin"
libdir     = "/home/dragemil/.cabal/lib/x86_64-linux-ghc-8.8.2/haskell-companion-0.1.0.0-inplace"
dynlibdir  = "/home/dragemil/.cabal/lib/x86_64-linux-ghc-8.8.2"
datadir    = "/home/dragemil/.cabal/share/x86_64-linux-ghc-8.8.2/haskell-companion-0.1.0.0"
libexecdir = "/home/dragemil/.cabal/libexec/x86_64-linux-ghc-8.8.2/haskell-companion-0.1.0.0"
sysconfdir = "/home/dragemil/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_companion_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_companion_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_companion_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_companion_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_companion_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_companion_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
