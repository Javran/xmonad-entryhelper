{-# LANGUAGE FlexibleContexts #-}
module XMonad.Util.MainHelper where

import XMonad.Core hiding (recompile,config)
import XMonad.Main

import Data.Functor
import Control.Applicative
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
import Data.List
import System.Posix.User

import qualified XMonad.Config as XMC

data Config = Config
  { execute :: IO ()
  , recompileCommand :: IO (FilePath, [String], Maybe FilePath)
  , upToDateCheck :: IO Bool
  , binaryPath :: IO FilePath
  }

defaultUpToDateCheck :: IO Bool
defaultUpToDateCheck = do
    dir <- getXMonadDir
    let binn = "xmonad-"++arch++"-"++os
        bin  = dir </> binn
        base = dir </> "xmonad"
        src  = base ++ ".hs"
        lib  = dir </> "lib"
    libTs <- mapM getModTime . Prelude.filter isSource =<< allFiles lib
    srcT <- getModTime src
    binT <- getModTime bin
    return $ not $ any (binT <) (srcT : libTs)
  where
    getModTime f = catch (Just <$> getModificationTime f) (\(SomeException _) -> return Nothing)
    isSource = flip elem [".hs",".lhs",".hsc"] . takeExtension
    allFiles t = do
        let prep = map (t</>) . Prelude.filter (`notElem` [".",".."])
        cs <- prep <$> catch (getDirectoryContents t) (\(SomeException _) -> return [])
        ds <- filterM doesDirectoryExist cs
        concat . ((cs \\ ds):) <$> mapM allFiles ds

defaultConfig :: Config
defaultConfig = config
    where
      config = Config
       { execute = xmonad XMC.defaultConfig
       , recompileCommand = do
             dir <- getXMonadDir
             binn <- binaryPath config
             return ( "ghc"
                    , [ "--make", "xmonad.hs"
                      , "-i", "-ilib", "-fforce-recomp"
                      , "-v0"
                      , "-o", binn]
                    , Just dir)
       , upToDateCheck = defaultUpToDateCheck
       , binaryPath = do
           dir <- getXMonadDir
           return $ dir </> "xmonad-"++arch++"-"++os
       }

withHelper :: (Read (l Window), LayoutClass l Window) => XConfig l -> IO ()
withHelper xconf = withCustomHelper conf
   where
     conf = defaultConfig { execute = xmonad xconf }

withCustomHelper :: Config -> IO ()
withCustomHelper conf = do
    installSignalHandlers
    args <- getArgs
    let launch = catchIO (buildAndLaunch conf) >> execute conf
    case args of
        []                    -> launch
        ("--resume":_)        -> launch
        ["--help"]            -> printUsage >> exitFailure
        ["--recompile"]       -> withLock (recompile conf) >>= exitWith
        ["--replace"]         -> launch
        ["--restart"]         -> sendRestart
        ["--version"]         -> printVersion False
        ["--verbose-version"] -> printVersion True
        _                     -> printUsage >> exitFailure

buildAndLaunch :: Config -> IO ()
buildAndLaunch c = do
    upToDate <- upToDateCheck c
    if upToDate
      then void launch
      else do ec <- withLock (recompile c)
              case ec of
                ExitSuccess -> void launch
                ExitFailure v -> do
                  putStrLn $ "recompile failed with exitcode " ++ show v
                  exitFailure
  where
    launch = executeFile <$> binaryPath c
                         <*> pure False
                         <*> getArgs
                         <*> pure Nothing

recompile :: Config -> IO ExitCode
recompile c = do
    dir <- getXMonadDir
    let base = dir </> "xmonad"
        err  = base ++ ".errors"
    (cmd,args,cmpDir) <- recompileCommand c
    uninstallSignalHandlers
    status <- bracket (openFile err WriteMode) hClose $ \h ->
            waitForProcess =<< runProcess cmd args cmpDir Nothing
                                   Nothing (Just h) (Just h)
    installSignalHandlers
    return status

printUsage :: IO ()
printUsage = putStrLn "TODO"

printVersion :: Bool -> IO ()
printVersion _ = putStrLn "TODO"

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

withLock :: IO ExitCode -> IO ExitCode
withLock action = do
    tmpDir <- getTemporaryDirectory
    usr <- getLoginName
    let lockFile = tmpDir </> intercalate "." ["xmonad",usr,"lock"]
    withFileLock lockFile action

withFileLock :: FilePath -> IO ExitCode -> IO ExitCode
withFileLock fPath action = do
    lock <- doesFileExist fPath
    if lock
      then skipCompile
      else doCompile
  where
    skipCompile = do
        putStrLn $ "Lock file " ++ fPath ++ " found, aborting ..."
        putStrLn   "Delete lock file to continue."
        return (ExitFailure 1)
    doCompile = bracket (writeFile fPath "")
                        (const (removeFile fPath))
                        (const action)
