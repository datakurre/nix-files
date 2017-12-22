module Main (main) where

import XMonad

import qualified Data.Map as M
import Graphics.X11.Xlib
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.ThreeColumns

main :: IO ()
main = xmonad $ defaultConfig
  { startupHook = setWMName "LG3D"
  , modMask = mod4Mask
  , focusFollowsMouse = False
  , clickJustFocuses = True
  , keys = myKeys <+> keys defaultConfig
  , layoutHook = myLayout
  }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
  [ ((modm .|. shiftMask, xK_x), shellPrompt myXPConfig)
  ]

-- XPConfig options:
myXPConfig = defaultXPConfig
  { font = "xft:DejaVuSansMonoBook:pixelsize=26"
  , height = 32
  }

myLayout = tiled ||| threeCol
  where
    tiled = Tall nmaster delta ratio
    threeCol = ThreeCol nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 2/100
