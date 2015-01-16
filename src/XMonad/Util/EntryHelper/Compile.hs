module XMonad.Util.EntryHelper.Compile
  ( defaultCompile
  , defaultPostCompile
  , compileUsingShell
  ) where

import Control.Applicative
import System.Exit
import System.IO
import System.Posix.Process
import System.Process

import XMonad.Util.EntryHelper.File

defaultCompile :: Bool -> IO ExitCode
defaultCompile force = do
    b <- isSourceNewer
    if force || b
      then do
        bin <- binPath <$> getXMonadPaths
        let cmd = "ghc --make xmonad.hs -i -ilib -fforce-recomp -o " ++ bin
        compileUsingShell cmd
      else return ExitSuccess

defaultPostCompile :: ExitCode -> IO ()
defaultPostCompile ExitSuccess = return ()
defaultPostCompile st@(ExitFailure _) = do
    err <- getXMonadLog
    ghcErr <- readFile err
    src <- getXMonadSrc
    let msg = unlines $
              [ "Error detected while loading xmonad configuration file: " ++ src]
              ++ lines (if null ghcErr then show st else ghcErr)
              ++ ["","Please check the file for errors."]
    hPutStrLn stderr msg
    _ <- forkProcess $ executeFile "xmessage" True ["-default", "okay", msg] Nothing
    return ()

compileUsingShell :: String -> IO ExitCode
compileUsingShell cmd = do
    dir <- getXMonadDir
    compileLogPath <- getXMonadLog
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
