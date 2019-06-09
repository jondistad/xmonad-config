import XMonad
import XMonad.Actions.ShowText (ShowTextConfig(..), flashText, handleTimerEvent)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Timer (startTimer)

main = xmonad (def { terminal = "urxvt"
                   , modMask = mod4Mask
                   , handleEventHook = handleTimerEvent
                   }
                `additionalKeysP` myKeys)
  where
    myKeys = [("M-x x", flashText stc 1 "M-x x")]
    stc = def { st_font = "xft:FantasqueSansMono:size=18"
              , st_bg   = "black"
              , st_fg   = "white"
              }
