module JExt.OSD where

import XMonad
import XMonad.Actions.ShowText

osdTextConfig :: ShowTextConfig
osdTextConfig = STC { st_font = "xft:FantasqueSansMono:size=48"
                    , st_bg   = "black"
                    , st_fg   = "white"
                    }

osdShow :: String -> X ()
osdShow = flashText osdTextConfig 1
