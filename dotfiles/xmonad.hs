module Main (main) where

import XMonad

import qualified Data.Map as M
import Graphics.X11.Xlib
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Layout.GridVariants
import XMonad.Layout.ResizableTile
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
  , ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
  , ((modm .|. shiftMask, xK_l), sendMessage MirrorExpand)
  , ((modm .|. shiftMask, xK_a), sendMessage (IncMasterN 1))
  , ((modm .|. shiftMask, xK_z), sendMessage (IncMasterN (-1)))
  ]

-- XPConfig options:
myXPConfig = def
  { font = "xft:DejaVuSansMonoBook:pixelsize=26"
  , height = 32
  }

myLayout = tall ||| threeCol ||| split
  where
    tall = ResizableTall nmaster delta ratio []
    threeCol = ThreeCol nmaster delta ratio
    split = Mirror (SplitGrid XMonad.Layout.GridVariants.L 3 1 (3/4) (3/4) delta)
    nmaster = 1
    ratio = 1/2
    delta = 1/100
