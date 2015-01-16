module XMonad.Util.EntryHelper.Entry
  ( withHelper
  ) where

import XMonad.Core hiding (recompile,config)

import System.Environment
import System.Exit
import System.Info
import Data.Version (showVersion)
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
