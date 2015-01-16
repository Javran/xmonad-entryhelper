{-# LANGUAGE FlexibleContexts, CPP #-}
module XMonad.Util.MainHelper where

import XMonad.Core hiding (recompile,config)
import XMonad.Main

import Data.Functor
import Control.Exception.Extensible
import Control.Monad
import System.Environment
import System.Exit
import System.Posix.Process
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.FilePath
import System.IO
import System.Info
import System.Process
import System.Directory
import Data.Version (showVersion)
import Data.List
import System.Posix.User
import qualified XMonad.Config as XMC
import Graphics.X11.Xinerama (compiledWithXinerama)

data Config a = Config
  { run         :: IO ()
  , compile     :: Bool -> IO a
  , postCompile :: a -> IO ()
  }

defaultConfig :: Config ExitCode
defaultConfig = Config
  { run = xmonad XMC.defaultConfig
  , compile = defaultCompile
  , postCompile = defaultPostCompile
  }

-- | gets the absolute path to the xmonad binary file.
getXMonadBin :: IO FilePath
getXMonadBin =  (</> "xmonad-"++arch++"-"++os)
            <$> getXMonadDir

-- | get the absolute path to the xmonad compile log.
getXMonadCompileLog :: IO FilePath
getXMonadCompileLog =  (</> "xmonad.errors")
                 <$> getXMonadDir

getXMonadSrc :: IO FilePath
getXMonadSrc = (</> "xmonad.hs")
            <$> getXMonadDir

xmonadVersion :: String
xmonadVersion = VERSION_xmonad

withHelper :: Config a -> IO ()
withHelper cfg = do
    args <- getArgs
    let launch = installSignalHandlers >> run cfg
    case args of
        []                    -> launch
        ("--resume":_)        -> launch
        ["--help"]            -> printHelp
        ["--recompile"]       -> compile cfg True >>= postCompile cfg
        ["--replace"]         -> launch
        ["--restart"]         -> sendRestart
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

compileUsingShell :: String -> IO ExitCode
compileUsingShell cmd = do
    dir <- getXMonadDir
    compileLogPath <- getXMonadCompileLog
    hNullInput <- openFile "/dev/null" ReadMode
    hCompileLog <- openFile compileLogPath WriteMode
    hSetBuffering hCompileLog NoBuffering
    let cp = (shell cmd)
               { cwd     = Just dir
               , std_in  = UseHandle hNullInput
               , std_out = UseHandle hCompileLog
               , std_err = UseHandle hCompileLog
               }
    (_,_,_,ph) <- createProcess cp
    waitForProcess ph

isSourceNewer :: IO Bool
isSourceNewer = do
    dir <- getXMonadDir
    bin <- getXMonadBin
    let lib = dir </> "lib"
        base = dir </> "xmonad"
        src  = base ++ ".hs"
    libTs <- mapM getModTime . filter isSource =<< allFiles lib
    srcT <- getModTime src
    binT <- getModTime bin
    -- should be at least one element in (srcT: libTs)
    -- and "Just _" is always greater than "Nothing"
    -- therefore, this procedure returns true when one of the following happens:
    -- - when the binary file doesn't exist
    -- - when there are some source files newer than the binary
    return $ any (binT <) (srcT : libTs)
  where
    getModTime fName = safeIO Nothing (Just <$> getModificationTime fName)
    isSource = (`elem` words ".hs .lhs .hsc") . takeExtension
    allFiles t = do
        let prep = map (t </>) . filter (`notElem` [".", ".."])
        cs <- prep <$> safeIO [] (getDirectoryContents t)
        ds <- filterM doesDirectoryExist cs
        concat . ((cs \\ ds):) <$> mapM allFiles ds

safeIO :: a -> IO a -> IO a
safeIO def action =
    catch action (\(SomeException _) -> return def)

defaultCompile :: Bool -> IO ExitCode
defaultCompile force = do
    b <- isSourceNewer
    if force || b
      then do
        bin <- getXMonadBin
        let cmd = "ghc --make xmonad.hs -i -ilib -fforce-recomp -o " ++ bin
        compileUsingShell cmd
      else return ExitSuccess

defaultPostCompile :: ExitCode -> IO ()
defaultPostCompile ExitSuccess = return ()
defaultPostCompile st@(ExitFailure _) = do
    err <- getXMonadCompileLog
    ghcErr <- readFile err
    src <- getXMonadSrc
    let msg = unlines $
              [ "Error detected while loading xmonad configuration file: " ++ src]
              ++ lines (if null ghcErr then show st else ghcErr)
              ++ ["","Please check the file for errors."]
    hPutStrLn stderr msg
    _ <- forkProcess $ executeFile "xmessage" True ["-default", "okay", msg] Nothing
    return ()

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
