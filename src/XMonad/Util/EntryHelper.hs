{-# LANGUAGE FlexibleContexts #-}
module XMonad.Util.EntryHelper where

import XMonad.Core hiding (recompile,config)

import Control.Exception.Extensible
import System.Environment
import System.Exit
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.FilePath
import System.Info
import System.Directory
import Data.Version (showVersion)
import Data.List
import System.Posix.User
import Graphics.X11.Xinerama (compiledWithXinerama)

import XMonad.Util.EntryHelper.Generated
import XMonad.Util.EntryHelper.Util
import XMonad.Util.EntryHelper.Config

withHelper :: Config a -> IO ()
withHelper cfg = do
    args <- getArgs
    let launch = installSignalHandlers >> run cfg
        recompile = compile cfg True >>= postCompile cfg
    case args of
        []                    -> launch
        ("--resume":_)        -> launch
        ["--help"]            -> printHelp
        ["--recompile"]       -> recompile
        ["--replace"]         -> launch
        ["--restart"]         -> safeIO () recompile >> sendRestart
        ["--version"]         -> putStrLn $ unwords shortVersion
        ["--verbose-version"] -> putStrLn . unwords $ shortVersion ++ longVersion
        _                     -> printHelp >> exitFailure
 where
    shortVersion = [ "xmonad", xmonadVersion ]
    longVersion  = [ "compiled by", compilerName, showVersion compilerVersion
                   , "for",  arch ++ "-" ++ os
                   , "\nXinerama:", show compiledWithXinerama ]

printHelp :: IO ()
printHelp = do
    self <- getProgName
    putStr . unlines $
      [ "xmonad-entryhelper - XMonad config entry point wrapper"
      , ""
      , "Usage: " ++ self ++ " [OPTION]"
      , "Options:"
      , "  --help                       Print this message"
      , "  --version                    Print XMonad's version number"
      , "  --recompile                  Recompile XMonad"
      , "  --replace                    Replace the running window manager with XMonad"
      , "  --restart                    Request a running XMonad process to restart"
      ]

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

withLock :: a -> IO a -> IO a
withLock def action = do
    tmpDir <- getTemporaryDirectory
    -- https://ghc.haskell.org/trac/ghc/ticket/1487
    -- avoid using "getLoginName" here
    usr <- getEffectiveUserName
    let lockFile = tmpDir </> intercalate "." ["xmonad",usr,"lock"]
    withFileLock lockFile def action

withFileLock :: FilePath -> a -> IO a -> IO a
withFileLock fPath def action = do
    lock <- doesFileExist fPath
    if lock
      then skipCompile
      else doCompile
  where
    skipCompile = do
        putStrLn $ "Lock file " ++ fPath ++ " found, aborting ..."
        putStrLn   "Delete lock file to continue."
        return def
    doCompile = bracket (writeFile fPath "")
                        (const (removeFile fPath))
                        (const action)
