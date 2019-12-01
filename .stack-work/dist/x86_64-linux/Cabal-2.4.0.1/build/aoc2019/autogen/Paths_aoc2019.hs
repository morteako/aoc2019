{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_aoc2019 (
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

bindir     = "/mnt/c/Users/MortenAske/aoc2019/.stack-work/install/x86_64-linux/33e77a9beda42c4cfdf436d3a7ee456c1dfdd737ce8f8af72a9dcc0622349908/8.6.5/bin"
libdir     = "/mnt/c/Users/MortenAske/aoc2019/.stack-work/install/x86_64-linux/33e77a9beda42c4cfdf436d3a7ee456c1dfdd737ce8f8af72a9dcc0622349908/8.6.5/lib/x86_64-linux-ghc-8.6.5/aoc2019-0.1.0.0-6eHqEx1YiiiFooNDmJjdLT-aoc2019"
dynlibdir  = "/mnt/c/Users/MortenAske/aoc2019/.stack-work/install/x86_64-linux/33e77a9beda42c4cfdf436d3a7ee456c1dfdd737ce8f8af72a9dcc0622349908/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/mnt/c/Users/MortenAske/aoc2019/.stack-work/install/x86_64-linux/33e77a9beda42c4cfdf436d3a7ee456c1dfdd737ce8f8af72a9dcc0622349908/8.6.5/share/x86_64-linux-ghc-8.6.5/aoc2019-0.1.0.0"
libexecdir = "/mnt/c/Users/MortenAske/aoc2019/.stack-work/install/x86_64-linux/33e77a9beda42c4cfdf436d3a7ee456c1dfdd737ce8f8af72a9dcc0622349908/8.6.5/libexec/x86_64-linux-ghc-8.6.5/aoc2019-0.1.0.0"
sysconfdir = "/mnt/c/Users/MortenAske/aoc2019/.stack-work/install/x86_64-linux/33e77a9beda42c4cfdf436d3a7ee456c1dfdd737ce8f8af72a9dcc0622349908/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aoc2019_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aoc2019_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aoc2019_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aoc2019_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc2019_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc2019_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
