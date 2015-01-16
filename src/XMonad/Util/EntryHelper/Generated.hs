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
