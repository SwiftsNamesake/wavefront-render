{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_wavefront_render (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\wavefront-render\\.stack-work\\install\\26812796\\bin"
libdir     = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\wavefront-render\\.stack-work\\install\\26812796\\lib\\x86_64-windows-ghc-8.0.1\\wavefront-render-0.1.0.0-FvMeJIVTVI2CdYT5fdVjPa"
datadir    = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\wavefront-render\\.stack-work\\install\\26812796\\share\\x86_64-windows-ghc-8.0.1\\wavefront-render-0.1.0.0"
libexecdir = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\wavefront-render\\.stack-work\\install\\26812796\\libexec"
sysconfdir = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\wavefront-render\\.stack-work\\install\\26812796\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wavefront_render_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wavefront_render_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wavefront_render_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wavefront_render_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wavefront_render_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
