{-|
  Copyright   : (c) 2015 Javran Cheng
  License     : MIT
  Maintainer  : Javran.C@gmail.com
  Stability   : unstable
  Portability : non-portable (requires X11)

  Re-exporting submodules

-}
module XMonad.Util.EntryHelper
  ( module XMonad.Util.EntryHelper.Util
  , module XMonad.Util.EntryHelper.Compile
  , module XMonad.Util.EntryHelper.File
  , module XMonad.Util.EntryHelper.Config
  , module XMonad.Util.EntryHelper.Generated
  ) where

import XMonad.Util.EntryHelper.Util
import XMonad.Util.EntryHelper.Compile
import XMonad.Util.EntryHelper.File
import XMonad.Util.EntryHelper.Config
import XMonad.Util.EntryHelper.Generated

-- import/export shortcut is not justified here
-- because it blocks document generation

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
