module Main (main) where

import XMonad

import qualified Data.Map as M
import Graphics.X11.Xlib
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Layout.Dwindle
import XMonad.Layout.GridVariants
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.EwmhDesktops
import System.Posix.Env (putEnv)

main :: IO ()
main = do
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"
  xmonad $ ewmh def
    { modMask = mod4Mask
    , focusFollowsMouse = False
    , clickJustFocuses = True
    , keys = myKeys <+> keys def
    , layoutHook = myLayout
    , terminal = "xterm"
    }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
  [ ((modm .|. shiftMask, xK_x), shellPrompt myXPConfig)
  ]

-- XPConfig options:
myXPConfig = def
  { font = "xft:DejaVuSansMonoBook:pixelsize=26"
  , height = 32
  }

myLayout = tiled ||| threeCol ||| split ||| spiral
  where
    tiled = Tall nmaster delta ratio
    threeCol = ThreeCol nmaster delta ratio
    split = Mirror (SplitGrid XMonad.Layout.GridVariants.L 3 1 (3/4) (3/4) (0.5/100))
    spiral = Spiral XMonad.Layout.Dwindle.L CW 1.1 1.1
    nmaster = 1
    ratio = 1/2
    delta = 2/100
