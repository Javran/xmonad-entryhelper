{-|
  Copyright   : (c) 2015 Javran Cheng
  License     : MIT
  Maintainer  : Javran.C@gmail.com
  Stability   : unstable
  Portability : non-portable (requires X11)

  Information generated from cabal macros

-}
{-# LANGUAGE CPP #-}
module XMonad.Util.EntryHelper.Generated
  ( xmonadVersion
  ) where

-- | the XMonad version compiled with this library
xmonadVersion :: String
#ifdef VERSION_xmonad
xmonadVersion = VERSION_xmonad
#else
xmonadVersion = "(unknown version)"
#endif
