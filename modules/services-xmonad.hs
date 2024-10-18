module Main (main) where

import XMonad

import qualified Data.Map as M
import Graphics.X11.Xlib
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Layout.Dwindle
import XMonad.Layout.GridVariants
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W
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

--Looks to see if focused window is floating and if it is the places it in the stack
--else it makes it floating but as full screen
toggleFull = withFocused (\windowId -> do
{
   floats <- gets (W.floating . windowset);
   if windowId `M.member` floats
   then do
       withFocused $ toggleBorder
       withFocused $ windows . W.sink
   else do
       withFocused $ toggleBorder
       withFocused $  windows . (flip W.float $ W.RationalRect 0 0 1 1)    }    )


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
  [ ((modm .|. shiftMask, xK_x), shellPrompt myXPConfig)
  , ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
  , ((modm .|. shiftMask, xK_l), sendMessage MirrorExpand)
  , ((modm .|. shiftMask, xK_a), sendMessage (IncMasterN 1))
  , ((modm .|. shiftMask, xK_z), sendMessage (IncMasterN (-1)))
  -- toggle between full screen and tiling
  , ((modm, xK_f              ), toggleFull)
  , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set 5%+")
  , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 5%-")
  , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%- unmute")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+ unmute")
  , ((0, xF86XK_AudioMicMute), spawn "amixer set Capture toggle")
  , ((0, xF86XK_Favorites), spawn "xlock -mode xjack")
  , ((0, xK_Cancel), spawn "sudo systemctl suspend")
  ]

-- XPConfig options:
myXPConfig = def
  { font = "xft:DejaVuSansMonoBook:pixelsize=26"
  , height = 32
  }

myLayout = tall ||| threeCol ||| split ||| spiral
  where
    tall = ResizableTall nmaster delta ratio []
    threeCol = ThreeCol nmaster delta ratio
    split = Mirror (SplitGrid XMonad.Layout.GridVariants.L 3 1 (3/4) (3/4) delta)
    spiral = Spiral XMonad.Layout.Dwindle.L CW 1.1 1.1
    nmaster = 1
    ratio = 1/2
    delta = 1/100
