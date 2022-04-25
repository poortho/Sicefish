{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sicefish (
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

bindir     = "D:\\Users\\Elvin Liu\\Desktop\\hq\\488b\\Sicefish\\.stack-work\\install\\446830ca\\bin"
libdir     = "D:\\Users\\Elvin Liu\\Desktop\\hq\\488b\\Sicefish\\.stack-work\\install\\446830ca\\lib\\x86_64-windows-ghc-8.10.4\\sicefish-0.1.0.0-JxynDWSv7JSGRmWiOUJd29"
dynlibdir  = "D:\\Users\\Elvin Liu\\Desktop\\hq\\488b\\Sicefish\\.stack-work\\install\\446830ca\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "D:\\Users\\Elvin Liu\\Desktop\\hq\\488b\\Sicefish\\.stack-work\\install\\446830ca\\share\\x86_64-windows-ghc-8.10.4\\sicefish-0.1.0.0"
libexecdir = "D:\\Users\\Elvin Liu\\Desktop\\hq\\488b\\Sicefish\\.stack-work\\install\\446830ca\\libexec\\x86_64-windows-ghc-8.10.4\\sicefish-0.1.0.0"
sysconfdir = "D:\\Users\\Elvin Liu\\Desktop\\hq\\488b\\Sicefish\\.stack-work\\install\\446830ca\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sicefish_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sicefish_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sicefish_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sicefish_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sicefish_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sicefish_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
