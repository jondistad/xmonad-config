import           XMonad
import           XMonad.Actions.ShowText (ShowTextConfig(..), flashText, handleTimerEvent)
import           XMonad.Actions.Submap (submap, submapDefaultWithKey)
import           XMonad.Util.EZConfig (additionalKeysP, mkKeymap)
import           XMonad.Util.Timer (startTimer)

import           Graphics.X11.Xlib.Misc (keysymToString)

import           Data.Bits ((.&.), complement, finiteBitSize, shiftL)
import           Data.List (intercalate)
import qualified Data.Map.Strict as M
import           Data.Monoid (All(..))
import           Numeric (showHex)
import           System.IO (Handle, IOMode(..), openFile, hFlush, hPutStrLn)
import           System.IO.Unsafe (unsafePerformIO)

main = xmonad (def { terminal = "urxvt"
                   , modMask = mod4Mask
                   , handleEventHook = handleTimerEvent -- <+> debugKeyEvents
                   }
                `additionalKeysP` myKeys)
  where
    myKeys = [ ("M-x", modX)
             -- , ("C-S-x", flashText stc 1 "C-S-x")
             ]
    modX = do
      xconfig <- asks config
      flashText stc 1 "M-x"
      submapDefaultWithKey
        (\chd -> flashText stc 1 ("Undefined chord: M-x "++chordStr xconfig chd))
        (mkKeymap xconfig [("x", flashText stc 1 "M-x x")
                          ,("p", flashText stc 1 "M-x p")
                          ])
    chordStr c (mask, key) = maskStr c mask ++ keysymToString key
    masks c = [modMask c, controlMask, shiftMask] ++ iterate (`shiftL` 1) mod1Mask
    maskNames = ["M", "C", "S"] ++ map (('M':).(:[])) ['1'..'5']
    maskStr _ 0 = ""
    maskStr c m = foldr
                    (\(m', n) s -> if m .|. m' == m'
                                   then n++"-"++s
                                   else s)
                    "" (filter (\(m,s) -> s == "M" || m /= modMask c) (masks c `zip` maskNames))
                  
    stc = def { st_font = "xft:FantasqueSansMono:size=18"
              , st_bg   = "black"
              , st_fg   = "white"
              }

logfile :: Handle
logfile = unsafePerformIO $ openFile "/home/jon/.xmonad/debuglog" AppendMode

debugKeyEvents :: Event -> X All
debugKeyEvents (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
  | t == keyPress =
      withDisplay $ \dpy -> do
        sym <- io $ keycodeToKeysym dpy code 0
        msk <- cleanMask m
        nl <- gets numberlockMask
        flashText
          def { st_font = "xft:FantasqueSansMono:size=13"
              , st_bg   = "black"
              , st_fg   = "white"
              }
          1 $ intercalate " " ["keycode"
                              ,show code
                              ,"sym"
                              ,show sym
                              ," ("
                              ,hex sym
                              ," \""
                              ,keysymToString sym
                              ,"\") mask"
                              ,hex m
                              ,"(" ++ vmask nl m ++ ")"
                              ,"clean"
                              ,hex msk
                              ,"(" ++ vmask nl msk ++ ")"
                              ]
        return (All True)
debugKeyEvents _ = return (All True)


-- | Convenient showHex variant
hex :: (Integral n, Show n) => n -> String
hex v = "0x" ++ showHex v ""

-- | Convert a modifier mask into a useful string
vmask                 :: KeyMask -> KeyMask -> String
vmask numLockMask msk =  intercalate " " $
                         reverse $
                         fst $
                         foldr vmask' ([],msk) masks
    where
      masks = map (\m -> (m,show m)) [0..toEnum (finiteBitSize msk - 1)] ++
              [(numLockMask,"num"  )
              ,(   lockMask,"lock" )
              ,(controlMask,"ctrl" )
              ,(  shiftMask,"shift")
              ,(   mod5Mask,"mod5" )
              ,(   mod4Mask,"mod4" )
              ,(   mod3Mask,"mod3" )
              ,(   mod2Mask,"mod2" )
              ,(   mod1Mask,"mod1" )
              ]
      vmask'   _   a@( _,0)                = a
      vmask' (m,s)   (ss,v) | v .&. m == m = (s:ss,v .&. complement m)
      vmask'   _        r                  = r
