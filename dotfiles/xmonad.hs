module Main (main) where

import XMonad

import qualified Data.Map as M
import Graphics.X11.Xlib
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Layout.ThreeColumns
import System.Posix.Env (putEnv)

main :: IO ()
main = do
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"
  xmonad $ defaultConfig
    { modMask = mod4Mask
    , focusFollowsMouse = False
    , clickJustFocuses = True
    , keys = myKeys <+> keys defaultConfig
    , layoutHook = myLayout
    , terminal = "xterm"
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
