module XMonad.Util.EntryHelper.Config
  ( Config(..)
  , defaultConfig
  ) where

import System.Exit

import XMonad.Main
import qualified XMonad.Config as XMC

import XMonad.Util.EntryHelper.Compile

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
