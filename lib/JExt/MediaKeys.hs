module JExt.MediaKeys ( updateBrightness
                      , updateVolume
                      , AudioComponent(..)
                      , toggleMute
                      ) where

import           JExt.OSD
import           JExt.StrictIO

import           XMonad
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.Run

import           Data.Char (isDigit)
import           System.IO (readIO)
import           System.IO.Unsafe (unsafePerformIO)

-- Backlight Brightness

newtype Brightness = Brightness Int
instance ExtensionClass Brightness where
  initialValue = Brightness initialBrightness
    where
      initialBrightness = unsafePerformIO (readFile' brightnessFile >>= readIO)
  extensionType = StateExtension

brightnessFile :: FilePath
brightnessFile = "/sys/class/backlight/intel_backlight/brightness"

updateBrightness pct = do
  Brightness cur <- XS.get :: X Brightness
  let step = round $ fromIntegral maxBrightness / (100 / pct)
      new = (cur + step) `min` maxBrightness `max` 0
  XS.put (Brightness new)
  spawn $ "echo "++show new++" > "++brightnessFile
  let newPct = round $ 100 * (fromIntegral new / fromIntegral maxBrightness)
  osdShow ("brightness: "++show newPct++"%")

maxBrightness :: Int
maxBrightness = unsafePerformIO $
  readFile' "/sys/class/backlight/intel_backlight/max_brightness" >>= readIO

-- Volume

updateVolume :: Int -> X ()
updateVolume pct = do
  let verb = if pct >= 0
             then "increase"
             else "decrease"

  out <- runProcessWithInput "ponymix" ["--sink", verb, show (abs pct)] ""
  osdShow $ "Volume: "++(takeWhile isDigit out)++"%"
  return ()

data AudioComponent = Speaker | Mic deriving (Eq, Show)

toggleMute :: AudioComponent -> X ()
toggleMute comp = do
  let sourceOrSink = case comp of
                       Mic -> "--source"
                       Speaker -> "--sink"
      cmdBase = "ponymix "++sourceOrSink
  spawn (cmdBase++" toggle")
