{-# LANGUAGE DeriveDataTypeable #-}

import           Graphics.X11.ExtraTypes
import           System.IO

import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.ShowText (handleTimerEvent)
import           XMonad.Actions.WorkspaceNames
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Spacing
import           XMonad.Layout.WindowArranger
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.Font (Align(..), initXMF, releaseXMF, textExtentsXMF, textWidthXMF)
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Util.XUtils

import           JExt.MediaKeys

myConfig =
  docks $
    def { manageHook = manageDocks <+> manageHook def
        -- , layoutHook = myLayout
        , handleEventHook = handleTimerEvent <+> handleEventHook def
        , terminal = "urxvt"
        , workspaces = myWorkspaces
        , modMask = mod4Mask -- Rebind Mod to the Windows key
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
  [ ("M-S-<Right>", nextWS)
  , ("M-S-p", spawn "flameshot gui")
  , ("M-S-<Left>", prevWS)
  , ("M-S-y", showMessage)
  , ("M-C-x", shellPrompt def { font = "xft:FantasqueSansMono:size=13" })
  , ("M-C-l", spawn "xscreensaver-command -lock")
  ]
  where
    showMessage = do
      MW mmw <- XS.get :: X MessageWindow
      case mmw of
        Nothing -> do
          f <- initXMF "xft:FantasqueSansMono:size=48"
          d <- asks display
          let txt = "hello"
          width <- textWidthXMF d f txt
          (as, ds) <- textExtentsXMF f txt
          let height = as + ds
          mw <- createNewWindow
                (Rectangle 500 500
                 (fromIntegral width)
                 (fromIntegral height))
                Nothing "green" True
          XS.put (MW (Just mw))
          showWindow mw
          paintAndWrite mw f (fromIntegral width) (fromIntegral height) 0 "green" "" "white" "green" [AlignCenter] [txt]
          releaseXMF f
        Just mw -> do
          XS.put (MW Nothing)
          deleteWindow mw
      
data MessageWindow = MW (Maybe Window)
instance ExtensionClass MessageWindow where
  initialValue = MW Nothing
  extensionType = StateExtension

myWorkspaces = ["web", "frontend", "portal", "backend", "debug", "scratch"]

-- myLayout = windowArrange $ spacingWithEdge 4 $ avoidStruts $ emptyBSP
