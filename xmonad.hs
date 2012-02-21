--  vim: set foldmarker={{{,}}} foldlevel=0 foldmethod=marker :
-- imports {{{
import System.Exit -- Begin
import System.IO
import System.Posix.Unistd -- End
import XMonad
import XMonad.Actions.WindowGo (title, raiseMaybe, runOrRaise) --, (=?))
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp -- End
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ScreenCorners -- End
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.Renamed
import XMonad.Layout.Circle -- End
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window-- End
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.WindowProperties (getProp32s) -- End
import Data.Monoid
import Data.Char
import Data.List (isPrefixOf)-- End
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
-- }}}
-- Setttings {{{
--myWallpaper      = "~/Pictures/wallpaper/mono.jpg"
myBitmapsPath        = "/home/japrogramer/.xmonad/icons/"
myBorderWidth        = 1
myDzenFGColor        = myNormalFGColor
myDzenBGColor        = myNormalBGColor
myFont               = "-*-terminus-*-*-*-*-12*-*-*-*-*"
myfocusMouse         = True
myFocusedBorderColor = myNormalFGColor
myIconDir            = "/home/japrogramer/.xmonad/icons"
myNormalBorderColor  = myNormalBGColor
myNormalBGColor      = "#2e3436"
myNormalFGColor      = "#5B40BF"
myTerminal           = "urxvt"
-- }}}
-- Dzen configs {{{
myDzenEvents    = "-e 'button3=' "
myDzenGenOpts   = "-fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' -h '16' "
myWorkspaceBar  = "dzen2 -p -ta l -w 640 " ++ myDzenEvents  ++ myDzenGenOpts -- Status Bar
myConkyBar      = "dzen2 -p -ta r -x 640 -w 640 " ++ myDzenGenOpts          -- Conky Bar

myDzenPP = defaultPP { ppSep             = "^bg(" ++ myNormalBGColor ++ ")^r(1,15)^bg()"
                     , ppWsSep           = " "
                     , ppCurrent         = dzenColor myNormalBGColor myNormalFGColor . pad
                     , ppVisible         = dzenColor myNormalBGColor myNormalFGColor . pad
                     , ppHidden          = wrapBg myNormalBGColor . pad
                     , ppHiddenNoWindows = wrapBg myNormalBGColor
                     , ppTitle           = shorten 60 . (\y -> " " ++ wrapFg myNormalFGColor y) .
                                                        (\x -> (filter (`elem` range ) x))
                     , ppLayout          = dzenColor myNormalFGColor myNormalBGColor .
                                            (\x -> case x of
                                                "ResizableTall"        -> wrapBitmap "half.xbm"
                                                "Mirror ResizableTall" -> wrapBitmap "dish.xbm"
                                                "Full"                 -> wrapBitmap "full.xbm"
                                                "Circle"               -> wrapBitmap "scorpio.xbm"
                                                "IM ResizableTall"     -> "^p(5)#^p(5)"
                                                _                      -> pad x
                                            )
                     }
                        where
                            wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
                            wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
                            wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"
                            range = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [ ' ' ]
-- }}}
-- workspaces {{{
myWorkspaces :: [WorkspaceId]
myWorkspaces =  clickable . (map dzenEscape) $ ["λ","¥","ψ","δ","Σ","ζ","η","θ","¤"]
                    where clickable l = [  ws  | (i,ws) <- zip [1..] l, let n = if i == 10 then i else 0 ]
-- }}}
--myxpconfig {{{
myXPConfig =
    XPC { font                = myFont
        , autoComplete        = Just 1
        , bgColor             = myNormalBGColor
        , bgHLight            = myNormalFGColor
        , borderColor         = myNormalBorderColor
        , completionKey       = xK_Tab
        , defaultText         = []
        , fgColor             = myNormalFGColor
        , fgHLight            = myNormalBGColor
        , height              = 18
        , historySize         = 25
        , historyFilter       = id
        , promptBorderWidth   = 1
        , promptKeymap        = defaultXPKeymap
        , position            = Bottom
        , showCompletionOnTab = False
        , searchPredicate     = isPrefixOf
        }
-- }}}
-- modMask {{{
myModMask = mod4Mask
-- }}}
-- Key bindings. Add, modify or remove key bindings here.{{{
argumenu = "-b -nb '"++ myNormalBGColor ++"' -nf '#736AFF' -sb '#5B40BF' -sf '#736AFF' -fn '" ++ myFont ++"'"
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. mod3Mask    , xK_Return ) , spawn $ XMonad.terminal conf      ) -- Lterminal
    , ((modm                 , xK_p      ) , spawn ("dmenu_run " ++ argumenu   )  ) -- Ldmenu
    , ((modm                 , xK_minus  ) , spawn ("transset-df -a --dec .05" )  ) -- Ltransperancy
    , ((modm                 , xK_equal  ) , spawn ("transset-df -a --inc .05" )  ) -- Ltransperancy
    , ((modm                 , xK_0      ) , spawn ("transset-df -a -t "       )  ) -- Ltransperancy
    , ((modm                 , xK_F9     ) , spawn ("killall compton;sleep 1;compton" )  ) -- Lcompton
    , ((modm                 , xK_f      ) , runInTerm "-title ranger" "sh -c 'ranger'"  ) -- Lranger
    , ((modm .|. mod3Mask    , xK_o      ) , runInTerm "-title elinks" "sh -c 'elinks'"  ) -- Lelinks
    , ((modm .|. mod3Mask    , xK_e      ) , runInTerm "-title gvim" "sh -c 'gvim'"      ) -- Lgvim
    , ((modm .|. mod3Mask    , xK_f      ) , raiseMaybe (spawn "firefox") (checkName "Firefox") ) -- Lfirefox
    , ((modm .|. mod3Mask    , xK_t      ) , raiseMaybe (runInTerm "-title tty-clock" "sh -c 'tty-clock -sct'"      ) (title =? "tty-clock" )  ) -- Ltime
    , ((modm .|. mod3Mask    , xK_i      ) , raiseMaybe (runInTerm "-title irssi" "sh -c 'irssi'"                   ) (title =? "irssi"     )  ) -- Lirssi
    , ((modm .|. mod3Mask    , xK_m      ) , raiseMaybe (runInTerm "-title mocp" "sh -c 'mocp -T yellow_red_theme'" ) (title =? "mocp"      )  ) -- Lmocp
    , ((modm .|. mod3Mask    , xK_p      ) , spawn "pidgin"                ) -- Lpidgin
    , ((modm .|. mod3Mask    , xK_g      ) , windowPromptGoto myXPConfig   ) -- prompt
    , ((modm .|. mod3Mask    , xK_c      ) , kill                          ) -- close focused window
    , ((modm .|. mod3Mask    , xK_b      ) , windowPromptBring myXPConfig  ) -- prompt
    , ((modm .|. mod3Mask    , xK_n      ) , spawn "nautilus --no-desktop" ) -- Lnautalius
    , ((modm .|. mod3Mask    , xK_j      ) , windows W.swapDown            ) -- Swap the focused window with the next window
    , ((modm .|. mod3Mask    , xK_k      ) , windows W.swapUp              ) -- Swap the focused window with the previous window
    , ((modm .|. mod3Mask    , xK_bracketleft   ) , sendMessage $ Toggle REFLECTX ) -- REFLECTX Layout
    , ((modm .|. mod3Mask    , xK_bracketright  ) , sendMessage $ Toggle REFLECTY ) -- REFLECTY Layout
    , ((modm .|. mod3Mask    , xK_space  ) , setLayout $ XMonad.layoutHook conf   ) -- Reset the layouts on workspace
    , ((modm                 , xK_j      ) , windows W.focusDown    ) -- Move focus to the next window
    , ((modm                 , xK_k      ) , windows W.focusUp      ) -- Move focus to the previous window
    , ((modm                 , xK_n      ) , refresh                ) -- Resize viewed windows to the correct size
    , ((modm                 , xK_space  ) , sendMessage NextLayout ) -- Rotate Layout Algorithms
    , ((modm                 , xK_m      ) , windows W.focusMaster  ) -- Move focus to the master window
    , ((modm                 , xK_a      ) , warpToWindow (1/20) (19/20)) -- @@ Move pointer to currently focused window
    , ((modm                 , xK_s      ) , warpToWindow (19/20) (1/20)) -- @@ Move pointer to currently focused window
    , ((modm                 , xK_Tab    ) , windows W.focusDown    ) -- Move focus to the next window
    , ((modm                 , xK_Return ) , windows W.swapMaster   ) -- Swap the focused window and the master window
    , ((0                    , 0x1008ff17) , spawn "mocp -f"    ) -- XF86AudioNext mocp Next
    , ((0                    , 0x1008ff16) , spawn "mocp -r"    ) -- XF86AudioPrev mocp Previous
    , ((0                    , 0x1008ff14) , spawn "mocp -G"    ) -- XF86AudioPlay mocp Toggle
    --((0                    , 0x1008ff15) , spawn ""           ) -- XF86AudioStop mocp Stop
    , ((modm                 , xK_h      ) , sendMessage Shrink ) -- Shrink the master area
    , ((modm                 , xK_l      ) , sendMessage Expand ) -- Expand the master area
    , ((modm                 , xK_t      ) , withFocused $ windows . W.sink ) -- Push window back into tiling
    , ((modm                 , xK_comma  ) , sendMessage (IncMasterN 1)     ) -- Increment the number of windows in the master area
    , ((modm                 , xK_period ) , sendMessage (IncMasterN (-1))  ) -- Deincrement the number of windows in the master area
    , ((modm                 , xK_b      ) , sendMessage ToggleStruts       ) -- Toggle the status bar gap
    , ((modm              , xK_q         ) , spawn "killall conky dzen2; xmonad --recompile; xmonad --restart") -- Restart xmonad
    , ((modm .|. shiftMask, xK_q         ) , io $ exitWith ExitSuccess      ) --exit
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i )
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, mod3Mask )]]
    -- ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        -- | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        -- , (f, m) <- [(W.view, 0), (W.shift, mod3Mask )]]
-- }}}
-- Mouse bindings: default actions bound to mouse events {{{
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- floating mode and move by dragging
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster)) -- Raise the window to the top of the stack
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)) -- floating mode and resize by dragging
    -- mouse scroll wheel (button4 and button5)
    , ((modm, button4), (\w -> focus w >> windows W.swapUp))
    , ((modm, button5), (\w -> focus w >> windows W.swapDown))
    ]
-- }}}
-- Layouts: {{{
myLayout = avoidStruts                                   $
           onWorkspace (myWorkspaces !! 6 ) gimpLayouts  $
           onWorkspace (myWorkspaces !! 4 ) pidginLayout $
           myLayouts
               where
                    myLayouts    = mkToggle (single REFLECTX) $
                                   mkToggle (single REFLECTY) $ ( tiled ||| Mirror tiled ||| Circle ||| full )
                    pidginLayout = mkToggle (single REFLECTX) $ withIM (15/100) (Role "buddy_list") tiled
                    gimpLayouts  = gimpLayout ||| gimpLayout2 
                    gimpLayout   = renamed [Replace "gimp"] $ mkToggle (single REFLECTX) $
                                   withIM (0.13) (Role "gimp-toolbox") $
                                   reflectHoriz                        $
                                   withIM (0.17) (Role "gimp-dock") Full
                    gimpLayout2  = renamed [Replace "gimp"] $ mkToggle (single REFLECTX) $
                                   withIM (0.13) (Role "gimp-toolbox") $
                                   withIM (0.17) (Role "gimp-dock") Full
                    tiled        = smartBorders (ResizableTall nmaster delta ratio [])
                    full         = noBorders Full
                    nmaster      = 1
                    delta        = 3/100
                    ratio        = toRational goldenRatio
                    goldenRatio  = 2/(1+sqrt(5)::Double);
-- }}}
-- Window rules: {{{
-- To find the property name associated with a program, use xprop | grep WM_CLASS 
checkName  x = (className =? x <||> title =? x <||> resource =? x)

myManageHook = composeAll . concat $
    [ [checkName x --> doShift      (myWorkspaces!!0) | x <- my1Shifts]
    , [checkName x --> doShift      (myWorkspaces!!1) | x <- my2Shifts]
    , [checkName x --> doShift      (myWorkspaces!!2) | x <- my3Shifts]
    , [checkName x --> doShift      (myWorkspaces!!3) | x <- my4Shifts]
    , [checkName x --> doShiftAndGo (myWorkspaces!!4) | x <- my5Shifts]
    , [checkName x --> doShiftAndGo (myWorkspaces!!5) | x <- my6Shifts]
    , [checkName x --> doShiftAndGo (myWorkspaces!!6) | x <- my7Shifts]
    , [checkName x --> doShiftAndGo (myWorkspaces!!7) | x <- my8Shifts]
    , [checkName x --> doShiftAndGo (myWorkspaces!!8) | x <- my9Shifts]
    , [checkName x --> doFloat                        | x <- myTFloats]
    , [checkName x --> doIgnore                       | x <- myIgnores]
    , [isDialog    --> doFloat     ]
    , [isFullscreen --> doFullFloat]
    ]
        where
            doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
            myTFloats = ["Downloads", "XCalc", "Xmessage","Save As..."]
            myIgnores = []
            my1Shifts = []
            my2Shifts = ["Firefox"]
            my3Shifts = []
            my4Shifts = []
            my5Shifts = ["Pidgin"]
            my6Shifts = ["Wine"]
            my7Shifts = ["Gimp"]
            my8Shifts = []
            my9Shifts = []
-- }}}
-- fadehook {{{
myFadeHook = composeAll . concat $
    [ [checkName x --> transparency 0.0 | x <- myIgnores ]
    , [checkName x --> transparency 0.1 | x <- my1Opacity]
    , [checkName x --> transparency 0.2 | x <- my2Opacity]
    , [checkName x --> transparency 0.3 | x <- my3Opacity]
    , [checkName x --> transparency 0.4 | x <- my4Opacity]
    , [checkName x --> transparency 0.5 | x <- my5Opacity]
    , [checkName x --> transparency 0.6 | x <- my6Opacity]
    , [checkName x --> transparency 0.7 | x <- my7Opacity]
    , [checkName x --> transparency 0.8 | x <- my8Opacity]
    , [checkName x --> transparency 0.9 | x <- my9Opacity]
    , [isUnfocused --> transparency 0.2 | x <- myOpacityD]
    , [isDialog <&&> isUnfocused     --> transparency 0.1]
    , [isDialog                      --> transparency 0.1]
    , [checkDock                     --> transparency 0.2]
    ]
        where
            myOpacityD = myIgnores ++ my1Opacity ++ my2Opacity ++ my3Opacity
            myIgnores  = ["Firefox","Wine","gimp-image-window"]
            my1Opacity = ["Pidgin","gvim"]
            my2Opacity = ["gimp-toolbox","gimp-dock"]
            my3Opacity = ["mocp"]
            my4Opacity = []
            my5Opacity = []
            my6Opacity = []
            my7Opacity = []
            my8Opacity = []
            my9Opacity = []

myFadeHookHack = (liftX (fadeWindowsLogHook myFadeHook) >> idHook)
-- }}}
-- Event handling {{{
myEventHook = fullscreenEventHook <+> docksEventHook <+> screenCornerEventHook 
-- }}}
-- Status bars and logging {{{
logHook' h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }
-- }}}
-- Startup hook {{{
myStartupHook :: X ()
myStartupHook = do
                spawnOnce   " gnome-settings-daemon"
                spawnOnce   " nm-applet"
                --spawnOnce   " xsetroot -cursor_name plus -solid '#2e3436'"
                spawnOnce   " compton"
                --spawnOnce " compton -fF -I 0.025 -O 0.065 -D 1 -m 0.8 -i 0.6 -e 0.6"
                --spawnOnce   ( " xloadimage -onroot -fullscreen " ++ myWallpaper )
                addScreenCorners [ (SCUpperRight,nextWS)
                                 , (SCUpperLeft, prevWS)
                                 ]
-- }}}
-- Run xmonad {{{
main = do
    myStatusBarPipe <- spawnPipe myWorkspaceBar
    conckyBar <- spawnPipe ( "conky -c ~/.xmonad/conkyfd | " ++ myConkyBar)
    xmonad $ defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myfocusMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        handleEventHook    = myEventHook,
        layoutHook         = myLayout,
        startupHook        = myStartupHook,
        logHook            = logHook' myStatusBarPipe,
        manageHook         = manageDocks <+> myManageHook <+> myFadeHookHack
        }
-- }}}
