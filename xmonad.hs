{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Actions.Minimize
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar /home/dokkora/.xmonad/xmobarrc"
  xmonad $
    flip additionalKeys keysToAdd . flip removeKeys keysToRemove
      . docks
      $ desktopConfig
        { modMask = myMod,
          terminal = myTerminal,
          borderWidth = 3,
          focusedBorderColor = "purple",
          layoutHook = myLayout,
          startupHook = myStartup,
          manageHook = manageDocks <+> manageHook desktopConfig,
          logHook =
            dynamicLogWithPP $
              xmobarPP
                { ppOutput = hPutStrLn xmproc,
                  ppTitle = xmobarColor "green" "" . shorten 50
                }
        }

keysToAdd =
  [ ((myMod, xK_d), spawn rofiCommand),
    ((myMod, xK_Return), spawn myTerminal),
    ((myMod, xK_q), kill),
    ((myMod .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart"),
    ((myMod .|. shiftMask, xK_Return), windows W.swapMaster),
    ((myMod, xK_Caps_Lock), windows W.focusUp),
    ((myMod .|. shiftMask, xK_l), sendMessage Expand),
    ((myMod .|. shiftMask, xK_h), sendMessage Shrink),
    ((myMod .|. shiftMask, xK_k), sendMessage MirrorExpand),
    ((myMod .|. shiftMask, xK_j), sendMessage MirrorShrink),
    ((myMod, xK_x), withFocused minimizeWindow),
    ((myMod, xK_z), withLastMinimized maximizeWindowAndFocus),
    ((myMod, xK_l), windows W.swapDown),
    ((myMod, xK_h), windows W.swapUp),
    ((myMod, xK_backslash), spawn $ myTerminal ++ "-e ranger"),
    ((myMod, xK_Print), spawn "flameshot gui"),
    ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-"),
    ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+"),
    ((0, xF86XK_AudioMute), spawn "amixer set Master toggle"),
    ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
  ]

keysToRemove =
  []

myLayout = boringWindows (minimize $ avoidStruts resizableTall ||| fullscreen)
  where
    resizableTall = ResizableTall 1 (3 / 100) (1 / 2) []
    fullscreen = noBorders Full

myStartup = do
  spawn "flameshot &"

myTerminal = "alacritty"

myMod = mod4Mask

rofiCommand = "rofi -modi drun -show drun -line-padding 4 -columns 2 -padding 50 -hide-scrollbar -show-icons -drun-icon-theme \"Arc-X-D\" -font \"Droid Sans Regular 10\""
