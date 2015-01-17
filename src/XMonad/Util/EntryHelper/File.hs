module XMonad.Util.EntryHelper.File
  ( XMonadPaths(..)
  , getXMonadDir
  , getXMonadPaths
  , getXMonadBin
  , getXMonadLog
  , getXMonadSrc
  , getXMonadLibDir
  , isSourceNewer
  ) where

import Control.Applicative
import System.FilePath
import qualified XMonad.Core as XC
import System.Info
import System.Directory

import XMonad.Util.EntryHelper.Util

-- | XMonad default file paths
data XMonadPaths = XMonadPaths
  { dirPath    :: FilePath -- ^ directory path
  , binPath    :: FilePath -- ^ compiled binary path
  , logPath    :: FilePath -- ^ error log path
  , srcPath    :: FilePath -- ^ source file path (the path to "xmonad.hs")
  , libDirPath :: FilePath -- ^ directory path for library files
  } deriving (Show)

-- | gets information about XMonad-related paths
getXMonadPaths :: IO XMonadPaths
getXMonadPaths = (XMonadPaths
  <$> id                              -- dir
  <*> (</> "xmonad-"++arch++"-"++os)  -- bin
  <*> (</> "xmonad.errors")           -- log
  <*> (</> "xmonad.hs")               -- src
  <*> (</> "lib")) <$> getXMonadDir   -- lib

-- | gets information about XMonad-related paths, see also: 'XMonadPaths'
getXMonadBin, getXMonadLog, getXMonadSrc, getXMonadLibDir, getXMonadDir :: IO FilePath

getXMonadDir = XC.getXMonadDir
getXMonadBin = binPath <$> getXMonadPaths
getXMonadLog = logPath <$> getXMonadPaths
getXMonadSrc = srcPath <$> getXMonadPaths
getXMonadLibDir = libDirPath <$> getXMonadPaths

-- | returns true only when any of the followings is true:
--
--     * any of the source files under xmonad's default directory is newer than the binary
--     * the binary does not exist
isSourceNewer :: IO Bool
isSourceNewer = do
    paths <- getXMonadPaths
    let bin = binPath paths
        src = srcPath paths
        lib = libDirPath paths
    libTs <- mapM getModTime . filter isHaskellSourceFile =<< allFiles lib
    srcT <- getModTime src
    binT <- getModTime bin
    -- should be at least one element in (srcT: libTs)
    -- and "Just _" is always greater than "Nothing"
    -- therefore, this procedure returns true when one of the following happens:
    -- - when the binary file doesn't exist
    -- - when there are some source files newer than the binary
    return $ any (binT <) (srcT : libTs)
  where
    getModTime = safeIO' . getModificationTime
