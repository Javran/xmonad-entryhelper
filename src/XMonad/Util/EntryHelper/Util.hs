{-|
  Copyright   : (c) 2015 Javran Cheng
  License     : MIT
  Maintainer  : Javran.C@gmail.com
  Stability   : unstable
  Portability : non-portable (requires X11)

  Miscellaneous utilities for safe IO action, scanning files, sending X events, etc.

-}
module XMonad.Util.EntryHelper.Util
  ( safeIO
  , safeIO'
  , isHaskellSourceFile
  , allFiles
  , sendRestart
  ) where

import Control.Applicative
import Control.Monad
import Control.Exception.Extensible
import System.Directory
import System.FilePath
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Data.List

-- | performs an IO action and captures all the exceptions,
--   a default value is returned when there are exceptions.
safeIO :: a -> IO a -> IO a
safeIO def action =
    catch action (\(SomeException _) -> return def)

-- | performs an IO action and wraps the resulting value in Maybe
safeIO' :: IO a -> IO (Maybe a)
safeIO' action = safeIO Nothing (Just <$> action)

-- | checks if a file name is a Haskell source file
isHaskellSourceFile :: FilePath -> Bool
isHaskellSourceFile = (`elem` words ".hs .lhs .hsc") . takeExtension

-- | gets a list of all files under a given directory and its subdirectories
allFiles :: FilePath -> IO [FilePath]
allFiles t = do
    let prep = map (t </>) . filter (`notElem` [".", ".."])
    cs <- prep <$> safeIO [] (getDirectoryContents t)
    ds <- filterM doesDirectoryExist cs
    concat . ((cs \\ ds):) <$> mapM allFiles ds

-- | sends restart request to the current XMonad instance
sendRestart :: IO ()
sendRestart = do
    dpy <- openDisplay ""
    rw <- rootWindow dpy $ defaultScreen dpy
    xmonad_restart <- internAtom dpy "XMONAD_RESTART" False
    allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw xmonad_restart 32 0 currentTime
        sendEvent dpy rw False structureNotifyMask e
    sync dpy False
