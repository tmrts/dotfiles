import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.CycleWS

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import qualified XMonad.Util.Paste as Paste

import XMonad.Prompt
import XMonad.Prompt.Shell

import Data.List
import qualified XMonad.StackSet as W

import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Circle
import XMonad.Layout.Gaps

import XMonad.Layout.WorkspaceDir

import XMonad.Operations

import XMonad.Actions.CycleWS (prevWS, nextWS)

import System.IO
import System.Exit

{-myWorkspaces = [ "  ", " CODE ", " IRC ", " PDF "]-}
{-myWorkspaces = [ "1", "2", "3" ]-}

-- Colors, fonts and paths
colorBlack     = "#020202" --Background
colorBlackAlt  = "#1c1c1c" --Black Xdefaults
colorGray      = "#444444" --Gray
colorGrayAlt   = "#101010" --Gray dark
colorWhite     = "#a9a6af" --Foreground
colorWhiteAlt  = "#9d9d9d" --White dark
colorMagenta   = "#8e82a2"
colorBlue      = "#44aacc"
colorBlueAlt   = "#3955c4"
colorRed       = "#f7a16e"
colorRedAlt    = "#e0105f"
colorGreen     = "#66ff66"
colorGreenAlt  = "#558965"
boxLeftIcon    = "/home/tmrts/.icons/box_left.xbm"   --left icon of dzen logger boxes
boxLeftIcon2   = "/home/tmrts/.icons/box_left2.xbm"  --left icon2 of dzen logger boxes
boxRightIcon   = "/home/tmrts/.icons/box_right.xbm"  --right icon of dzen logger boxes
xRes           = 1366
yRes           = 768
panelHeight    = 16  --height of top and bottom panels
boxHeight      = 12  --height of dzen logger box
topPanelSepPos = 950 --left-right alignment pos of top panel
botPanelSepPos = 400 --left-right alignment pos of bottom panel

-- XPrompt Theme --
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
        { font              = dzenFont
        , bgColor           = colorBlack
        , fgColor           = colorWhite
        , bgHLight          = colorBlue
        , fgHLight          = colorBlack
        , borderColor       = colorGrayAlt
        , promptBorderWidth = 1
        , height            = panelHeight
        , position          = Top
        , historySize       = 100
        , historyFilter     = deleteConsecutive
        , autoComplete      = Nothing
        }

-- Startup Processes --
startup :: X()
startup = do
        {-spawnOnce "conky"-}
        {-spawnOnce "compton"-}
        spawnOnce "feh --bg-scale /home/tmrts/background.png"
        spawnOnce "xsetroot -cursor_name left_ptr"
        spawnOnce "xmodmap /home/tmrts/.xmodmaprc"

-- Layout --

fullScreen = noBorders (fullscreenFull Full)

emptyGaps = [(U, 42), (R, 8), (L, 8), (D, 8)]
resize = ResizableTall 1 (2/100) (1/2) []

layout = workspaceDir "~" resize ||| fullScreen ||| (gaps emptyGaps $ avoidStruts (spacing 6 resize)) -- ||| Circle

------------

dzenIcon name = "^i(/home/tmrts/.icons/" ++ name ++ ".xbm)"

logbar h = do
        dynamicLogWithPP $ tryPP h
tryPP :: Handle -> PP
tryPP h = defaultPP
        { ppOutput          = hPutStrLn h
        , ppCurrent         = dzenColor (fore) (blu1) . pad
        , ppVisible         = dzenColor (fore) (pink) . pad
        , ppHidden          = dzenColor (fore) (filledWS) . pad
        , ppHiddenNoWindows = dzenColor (fore) (back) . pad
        , ppUrgent          = dzenColor (fore) (red1) . pad
        , ppOrder           = \(ws:l:t) -> [ws,l]
        , ppSep             = " "
        , ppLayout          = dzenColor (fore) (red1) .
                                ( \t -> case t of
                        "Spacing 6 ResizableTall" -> " " ++ dzenIcon "stop"  ++ " " ++ "ResizableTall" ++ " "
                        "Circle"                  -> " " ++ dzenIcon "full"  ++ " " ++ "CIRCLE"        ++ " "
                        "Full"                    -> " " ++ dzenIcon "fox"   ++ " " ++ "FULL"          ++ " "
                        _                         -> " " ++ dzenIcon "pause" ++ " " ++ "TALL"          ++ " "
                                )
        }

blu1 = "#2196F3"
pink = "#528588"
red1 = "#BA5E57"
fore = "#DEE3E0"
back = "#343C48"
filledWS = "#43A047"

dzenFont = "Monospace-8"

dzenFormat = " -fn '" ++ dzenFont ++ "' -fg '" ++ fore ++ "' -bg '" ++ back ++ "'"

dzenLogBar = "dzen2 -xs 1 -ta l -x 130 -y 10 -h 24 -w 400 -e ''" ++ dzenFormat
dzenInfoBar = "conky -c /home/tmrts/.dzen-conkyrc | dzen2 -xs 1 -ta r -x 530 -y 10 -h 24 -w 1260 -e ''" ++ dzenFormat

-----------

sleep = "sleep 0.10" ++ " && "

main = do
    riceBar <- spawnPipe $ "conky -c /home/tmrts/.dzen-overunderrc | dzen2 -xs 1 -ta l -x 123 -y 5 -h 34 -w 1674 -e ''" ++ dzenFormat
    logBar  <- spawnPipe $ sleep ++ dzenLogBar
    infoBar <- spawnPipe $ sleep ++ dzenInfoBar

    xmonad $ ewmh defaultConfig
                { manageHook = manageDocks <+> manageHook defaultConfig
                , layoutHook = windowArrange layout
                , startupHook = startup
        {-, workspaces = myWorkspaces-}
                , modMask = mod4Mask
                , focusedBorderColor = "#2196F3"
                , normalBorderColor = "#6A555C"
        , logHook = logbar logBar
                , borderWidth = 2
                , terminal = "termite"
                } `additionalKeys`
        [ ((modMask,                 xK_x          ) , kill                                                                      ) -- %! Close the focused window
        , ((modMask .|. shiftMask,   xK_x          ) , io (exitWith ExitSuccess)                                                 ) -- %! Quit xmonad
        , ((modMask,                 xK_s          ) , spawn "rofi -show combi -bg '#343C48' -fg '#DEE3E0' -font 'Monospace 11'" ) -- %! Spawn system omni-search bar
        , ((modMask,                 xK_Tab        ) , toggleWS                                                                  ) -- %! Toggle the previous workspace
        , ((modMask,                 xK_Return     ) , spawn "termite  "                                                         )
        , ((modMask,                 xK_asciitilde ) , sendMessage NextLayout                                                    ) -- %! Rotate through the available layout algorithms
        {-, ((modMask .|. shiftMask,   xK_asciitilde), setLayout $ layoutHook conf) -- %! Reset the layouts on the current workspace to default-}
        {-, ((modMask .|. shiftMask,   xK_Tab   ), toggleWS) -- %! Pin a workspace to toggle-}
        {-, ((modMask .|. shiftMask,   xK_Tab   ), toggleWS) -- %! Send app to the previous workspace-}

        , ((noModMask, xK_F1                  ), spawn "emacsclient --create-frame")
        , ((noModMask .|. shiftMask, xK_F1    ), spawn "emacs")
        , ((noModMask, xK_F2                  ), spawn "chromium --cache-data-dir=/dev/shm/cache --process-per-site")
        , ((noModMask .|. shiftMask, xK_F2    ), spawn "chromium --incognito --cache-data-dir=/dev/shm/cache --process-per-site")
        , ((noModMask, xK_F6                  ), spawn "playerctl previous")
        , ((noModMask, xK_F7                  ), spawn "playerctl play-pause")
        , ((noModMask, xK_F8                  ), spawn "playerctl next")
        , ((noModMask, xK_F10                 ), spawn "amixer set Master toggle")
        , ((noModMask, xK_F11                 ), spawn "amixer set Master 3-")
        , ((noModMask .|. shiftMask, xK_F11   ), spawn "amixer set Master 50%")
        , ((noModMask, xK_F12                 ), spawn "amixer set Master 3+")
        , ((noModMask .|. shiftMask, xK_F12   ), spawn "amixer set Master 100%")
        , ((noModMask , xK_Print              ), spawn "scrot --select --quality 100 '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/crop/'")
        , ((noModMask .|. shiftMask, xK_Print ), spawn "scrot --focused --quality 100 '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/ss/'")
        {-, ((modMask .|. shiftMask,   xK_]   ), spawn "rofi -show ssh -bg '#343C48' -fg '#DEE3E0' -font 'Monospace 11' -ssh-command \"termite -e '{ssh-client} {host}'\"")-}
        ] where modMask = mod4Mask
