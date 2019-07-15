{-# LANGUAGE DeriveDataTypeable #-}

import           Graphics.X11.ExtraTypes

import           XMonad
import           XMonad.Actions.ShowText (handleTimerEvent)
import           XMonad.Hooks.DynamicLog (xmobar)
import           XMonad.Hooks.ManageDocks (docks, manageDocks)
import           XMonad.Prompt (XPConfig(..))
import           XMonad.Prompt.Shell (shellPrompt)
import           XMonad.Util.EZConfig (additionalKeys, additionalKeysP)

import           JExt.MediaKeys (AudioComponent(..), updateVolume, toggleMute, updateBrightness)

myConfig =
  docks $
    def { manageHook = manageDocks <+> manageHook def
        , handleEventHook = handleTimerEvent <+> handleEventHook def
        , terminal = "urxvt"
        , modMask = mod4Mask -- Rebind Mod to the Windows key
        , focusFollowsMouse = False
        }
    `additionalKeysP` myKeys
    `additionalKeys` extraKeys

main = do
  xmonad =<< xmobar myConfig

extraKeys =
  [ ((0, xF86XK_AudioLowerVolume), updateVolume (-4))
  , ((0, xF86XK_AudioRaiseVolume), updateVolume 4)
  , ((0, xF86XK_AudioMute), toggleMute Speaker)
  , ((0, xF86XK_AudioMicMute), toggleMute Mic)
  , ((0, xF86XK_MonBrightnessDown), updateBrightness (-4))
  , ((0, xF86XK_MonBrightnessUp), updateBrightness 4)
  ]

myKeys =
  [ ("M-S-p", spawn "flameshot gui")
  , ("M-S-x", shellPrompt def { font = "xft:FantasqueSansMono:size=13" })
  , ("M-S-l", spawn "xscreensaver-command -lock")
  ]
