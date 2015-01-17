module XMonad.Util.EntryHelper.Config
  ( Config(..)
  , defaultConfig
  , withHelper
  , withCustomHelper
  ) where

import System.Environment
import System.Exit
import System.Info
import Data.Version (showVersion)
import Graphics.X11.Xinerama (compiledWithXinerama)
import XMonad.Main
import XMonad.Core hiding (recompile)

import qualified XMonad.Config as XMC

import XMonad.Util.EntryHelper.Generated
import XMonad.Util.EntryHelper.Util
import XMonad.Util.EntryHelper.Compile

-- | the configuration for EntryHelper.
--
--   * @run@ should execute XMonad using a customized configuration.
--   * @compile force@ should compile the source file and return a value which
--     will lately be consumed by @postCompile@. @force@ is just a hint about whether
--     the compilation should be forced. @compile@ is free to ignore it and do up-to-date check
--     on its own.
--   * @postCompile val@ should take action according to the @val@, usually produced by @compile@
--
--   Note that:
--
--   * @compile@ should create a new process for compilation, as otherwise things like `executeFile`
--     will replace the current process image with a new process image, make it impossible
--     for @postCompile@ to invoke.
--   * @force@ is just a hint about whether the compilation should be forced.
--     and @compile@ is free to ignore it and do up-to-date checking on its own.
--   * don't remove the binary file when the compilation has failed, as XMonad restart relies
--     on it.
data Config a = Config
  { run         :: IO ()        -- ^ the action for executing XMonad
  , compile     :: Bool -> IO a -- ^ the action for compiling XMonad
  , postCompile :: a -> IO ()   -- ^ the action after compiling XMonad
  }

-- | default config for xmonad-entryhelper,
--   invokes xmonad with its default config file
defaultConfig :: Config ExitCode
defaultConfig = Config
  { run = xmonad XMC.defaultConfig
  , compile = defaultCompile
  , postCompile = defaultPostCompile
  }

{-
-- I don't think this is necessary,
-- as users can use "xmonad config" or just their main entries for "run"
-- depending on their needs.

class Executable r where
    execute :: r -> IO ()

instance (LayoutClass l Window, Read (l Window)) => Executable (XConfig l) where
    execute = xmonad

instance Executable a => Executable (IO a) where
    execute a = a >>= execute

instance Executable () where
    execute = const (return ())
-}

-- | @withHelper e@ is the same as calling `withCustomHelper` with default
--   @compile@ and @postCompile actions@
--
--   Either of the following will work:
--
--   * replace your main entry with @main = withHelper yourOldMain@
--   * use @main = withHelper (xmonad cfg)@ if you have only customized your `XConfig`
withHelper :: IO () -> IO ()
withHelper e = withCustomHelper defaultConfig { run = e }

-- | simulates the way that XMonad handles its command line arguments.
--
--   * when called with no argument, the action in @run@ will be used
--   * when called with a string prefixed with @"--resume"@, or when called with
--     @"--replace"@, the action in @run@ will be used
--   * when called with @"--recompile"@ or @"--restart"@, 'compile' will be called. And 'postCompile'
--     will handle the results from compliation.
--   * additionally when called with @"--restart"@ a restart request will be sent to the current
--     XMonad instance after the compilation regardless of the compilation result.
withCustomHelper :: Config a -> IO ()
withCustomHelper cfg = do
    -- since XMonad has hard-coded to call "xmonad --resume{..}" on restart
    -- I guess there isn't much we can do to have more command line functionalities
    -- TODO: a pontential plan might be:
    -- try to simulate "xmonad" when the program name is "xmonad",
    -- and otherwise an alternative command line argument parsing strategy will be applied.
    args <- getArgs
    let launch = installSignalHandlers >> run cfg
        recompile force = compile cfg force >>= postCompile cfg
    case args of
        []                    -> launch
        ("--resume":_)        -> launch
        ["--help"]            -> printHelp
        ["--recompile"]       -> recompile True
        ["--replace"]         -> launch
        ["--restart"]         -> safeIO () (recompile False) >> sendRestart
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
