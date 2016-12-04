--  vim: set foldmarker={{{,}}} foldlevel=0 foldmethod=marker :
-- imports {{{
import XMonad
import XMonad.Actions.Commands -- debug actions
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.NoBorders
import XMonad.Actions.RotSlaves
import XMonad.Actions.Submap
import XMonad.Actions.TagWindows
import XMonad.Actions.Warp
import XMonad.Actions.WithAll
import XMonad.Actions.WindowGo (title, raiseMaybe, runOrRaise) --, (=?)) -- End
{-import XMonad.Hooks.DebugKeyEvents-}
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.FadeInactive (fadeOut)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.ToggleHook
{-import QueryAnything-}
import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.XPropManage
{-import XMonad.Hooks.SetWMName -- End-}
import XMonad.Layout.Circle
import XMonad.Layout.CenteredMaster
import XMonad.Layout.IM
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile 
import XMonad.Layout.Spacing -- End
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window -- End
import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties (getProp32s) -- End
import System.Exit -- no Sort
import System.IO
import System.Posix.Unistd -- End
import Control.Monad (liftM2)
import Data.Char
import Data.List (isInfixOf,isPrefixOf) -- End
import Data.Monoid
import qualified Data.Map        as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
-- }}}
-- Setttings {{{
myBorderWidth        = 1
myfocusMouse         = True
myTerminal           = "urxvt"
myBGColor            = "#2e3436"
myFGColor            = "#5B40BF"
myModMask            = mod4Mask
myFocusedBorderColor = myFGColor
myNormalBorderColor  = myBGColor
{-xfontsel-}
myFont               = "-*-terminus-*-*-*-*-22-*-*-*-*-*-*-*"
myBoldFont           = "-*-terminus-bold-*-*-*-22-*-*-*-*-*-*-*"
myWorkspaces         = ["λ","¥","ψ","δ","Σ","ζ","η","θ","¤"]
myNotesPath          = "/home/archangel/Documents/Notes/NOTES"
myDmenu              = "-nb '" ++ myBGColor ++ "' -sb '" ++ myFGColor ++ "' -fn '" ++ myBoldFont ++ "' -b"
myDzenGenOpts        = "-fg '" ++ myFGColor ++ "' -bg '" ++ myBGColor ++ "' -fn '" ++ myBoldFont ++ "' -h '30' "
-- }}}
-- Dzen configs {{{
myWorkspaceBar = "dzen2 -p -ta l -y 2160 -w 1920 "        ++ myDzenGenOpts -- Status Bar
myConkyBar     = "dzen2 -p -ta r -y 2160 -x 1920 -w 1920 " ++ myDzenGenOpts -- Conky Bar

myDzenPP = defaultPP { ppSep             = "^bg(" ++ myBGColor ++ ")^r(1,15)^bg()"
                     , ppWsSep           = " "
                     , ppCurrent         = dzenColor myBGColor myFGColor . pad
                     , ppVisible         = dzenColor myBGColor myFGColor . pad
                     , ppHidden          = wrapBg myBGColor . mypad
                     , ppHiddenNoWindows = wrapBg myBGColor
                     , ppTitle           = shorten 60 . (\x -> wrapFg myFGColor $ filter (`elem` range ) x)
                     , ppLayout          = dzenColor myFGColor myBGColor . (\x -> case x of _ -> pad ">>=")
                     }
                        where
                            mypad = wrap "[" "]"
                            wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
                            wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
                            range = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [ ' ' ]
-- }}}
--myxpconfig {{{
myXPConfig =
    XPC { font                = myFont
        , defaultText         = []
        , autoComplete        = Just 1
        , alwaysHighlight     = True
        , position            = Bottom
        , showCompletionOnTab = False
        , promptBorderWidth   = 1
        , height              = 18
        , historySize         = 25
        , historyFilter       = id
        , completionKey       = xK_Tab
        , borderColor         = myFocusedBorderColor
        , promptKeymap        = defaultXPKeymap
        , bgColor             = myBGColor
        , fgColor             = myFGColor
        , bgHLight            = myFGColor
        , fgHLight            = myBGColor
        , searchPredicate     = isPrefixOf
        }
-- }}}
-- Key bindings {{{
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((0                  , 0x1008ff17      ) , spawn "mocp -f") -- XF86AudioNext
    , ((0                  , 0x1008ff16      ) , spawn "mocp -r") -- XF86AudioPrev
    , ((0                  , 0x1008ff15      ) , spawn "mocp -x") -- XF86AudioStop
    , ((0                  , 0x1008ff14      ) , do
                                                 withTaggedGlobalP "mocp" (W.shiftWin (myWorkspaces !! 3))
                                                 windows $ W.greedyView (myWorkspaces !! 3)
                                                 spawn "mocp -G") -- XF86AudioPlay
    , ((controlMask        , 0x1008ff14      ) , spawn "mocp -G") -- XF86AudioPlay
    , ((modm               , 0x1008ff14      ) , runInTerm "" "sh -c 'mocp -T darkdot_theme'") -- Lmocp temporary untill TODO1 above
    , ((modm               , xK_F12          ) , spawn "killall compton;sleep 1;compton") -- Lcompton
    , ((modm               , xK_minus        ) , spawn "transset-df -a --dec .05" ) -- Ltransperancy
    , ((modm               , xK_equal        ) , spawn "transset-df -a --inc .05" ) -- Ltransperancy
    , ((modm               , xK_0            ) , spawn "transset-df -a -t "       ) -- Ltransperancy
    , ((modm .|. shiftMask , xK_0            ) , submap . M.fromList $ -- Magic fadeOut
        [ ((m              , k               ) , f $ fadeOut (i/20)) -- Set opacity for all
            | (i, k) <- ( zip [20] [xK_0 ..] ) ++ ( zip [10..]  [xK_1 .. xK_9] ) -- [1..9] opacity range
            , (f, m) <- [(withAll, modm), (withFocused, 0)] -- Operation done on all or one
        ])
     --take a screenshot of entire display
    , ((modm               , xK_Print        ) , spawn "scrot ~/Pictures/screen_%Y-%m-%d-%H-%M-%S.png -d 1")
    --take a screenshot of focused window
    , ((modm .|. controlMask, xK_Print       ) , spawn "scrot ~/Pictures/window_%Y-%m-%d-%H-%M-%S.png -d 1-u") 
    , ((modm               , xK_comma        ) , sendMessage $ IncMasterN 1   ) -- Increment number of windows in master area
    , ((modm               , xK_period       ) , sendMessage $ IncMasterN $ -1) -- Deincrement number of windows in master area
    , ((modm               , xK_bracketleft  ) , sendMessage $ Toggle REFLECTX) -- REFLECTX Layout
    , ((modm               , xK_bracketright ) , sendMessage $ Toggle REFLECTY) -- REFLECTY Layout
    , ((modm               , xK_semicolon    ) , cycleRecentWS [xK_Alt_L] xK_semicolon xK_apostrophe) -- recent workspace
    , ((modm .|. mod3Mask  , xK_space        ) , setLayout   $ XMonad.layoutHook conf) -- Reset layout
    , ((modm .|. mod3Mask  , xK_Return       ) , spawn       $ XMonad.terminal   conf) -- Lterminal
    , ((modm .|. mod3Mask  , xK_t            ) , runInTerm "" "sh -c 'tmux'") -- LTERMINAL with tmux
    , ((modm .|. shiftMask , xK_t            ) , runInTerm "" "sh -c 'tmux attach'") -- LTERMINAL with tmux attach
    , ((modm .|. shiftMask , xK_Tab          ) , prevWS) -- change prevWorkSpace
    , ((modm               , xK_Tab          ) , nextWS) -- change nextWorkSpace
    , ((modm .|. shiftMask , xK_c            ) , kill ) -- Fast kill
    , ((modm .|. mod3Mask  , xK_c            ) , submap . M.fromList $ -- Speciall kills
        [ ( (modm          , k               ) , i )
            | (i, k) <- zip [killAll, kill, kill1, killAllOtherCopies] [xK_a, xK_k, xK_m, xK_o]
        ])
    , ((modm               , xK_c            ) , submap . M.fromList $
        [ ((modm           , k               ) , warpToWindow (x/20) (y/20)) -- Move pointer to focused window's corners
            | (k, x, y) <- zip3 [xK_w, xK_a, xK_s, xK_d, xK_f] [10,1,1,19,19] [10,19,1,1,19] -- corners and keys
        ])
    , ((modm .|. mod3Mask  , xK_e            ) , runInTerm "" "sh -c 'gvim'") -- Lgvim
    , ((modm .|. mod3Mask  , xK_f            ) , raiseMaybe (spawn "firefox") (checkName "Firefox")) -- Lfirefox
    , ((modm .|. mod3Mask  , xK_n            ) , spawn "nautilus --no-desktop") -- Lnautalius
    , ((modm .|. mod3Mask  , xK_g            ) , windowPromptGoto  myXPConfig ) -- prompt
    , ((modm .|. mod3Mask  , xK_b            ) , windowPromptBring myXPConfig ) -- prompt
    , ((modm .|. mod3Mask  , xK_j            ) , windows W.swapDown    ) -- Swap focused window with next window
    , ((modm .|. mod3Mask  , xK_k            ) , windows W.swapUp      ) -- Swap focused window with previous window
    , ((modm               , xK_Return       ) , windows W.swapMaster  ) -- Make focused window Master
    , ((modm               , xK_k            ) , windows W.focusUp     ) -- Move focus Up
    , ((modm               , xK_j            ) , windows W.focusDown   ) -- Move focus Down
    , ((modm               , xK_m            ) , windows W.focusMaster ) -- Focus  master window
    , ((modm               , xK_e            ) , submap . M.fromList $
        [ ((modm               , xK_o        ) , viewEmptyWorkspace ) -- Switch to Empty workspace
        , ((modm .|. shiftMask , xK_o        ) , tagToEmptyWorkspace) -- Move window to Empty workspace
        ])
    , ((modm               , xK_b            ) , submap . M.fromList $
        [ ((0              , xK_s            ) , withFocused toggleBorder ) -- toggleBorders, aesthetic
        , ((0              , xK_a            ) , withAll toggleBorder     ) -- toggleBorders, aesthetic
        ])
    , ((modm .|. shiftMask , xK_apostrophe   ) , toggleFullNext)
    , ((modm               , xK_v            ) , submap . M.fromList $
        [ ((modm           , xK_a            ) , windows copyToAll  ) -- @@ Make focused window always visible
        , ((modm           , xK_k            ) , killAllOtherCopies ) -- @@ Toggle window state back
        ])
    , ((modm               , xK_n            ) , refresh ) -- Resize viewed windows to the correct size
    , ((modm               , xK_space        ) , sendMessage NextLayout ) -- Next Layout
    , ((modm               , xK_h            ) , sendMessage Shrink     ) -- Shrink master area
    , ((modm               , xK_l            ) , sendMessage Expand     ) -- Expand master area
    , ((modm               , xK_f            ) , runInTerm "" "sh -c 'ranger'"  ) -- Lranger
    , ((modm               , xK_p            ) , spawn $ "dmenu_run " ++ myDmenu) -- Ldmenu
    , ((modm               , xK_t            ) , withFocused $ windows . W.sink ) -- Push window back into tiling
    , ((modm               , xK_g            ) , sendMessage $ ToggleStruts     ) -- Toggle the status bar gap
    , ((modm               , xK_q            ) , spawn "killall conky dzen2; xmonad --recompile; xmonad --restart") -- Restart xmonad
    , ((modm .|. shiftMask , xK_q            ) , io $ exitWith ExitSuccess) --exit
    , ((modm .|. shiftMask , xK_k            ) , rotSlavesUp   ) -- rotSlavesUp without shifting Master
    , ((modm .|. shiftMask , xK_j            ) , rotSlavesDown ) -- rotSlavesDown without shifting Master
    , ((modm .|. controlMask , xK_j          ) , rotAllDown    ) -- This is weird when Layout is Mirror'
    , ((modm .|. controlMask , xK_k          ) , rotAllUp      ) -- This is weird when Layout is Mirror'
    , ((modm .|. controlMask , xK_y          ) , commands >>= runCommand)
    , ((modm .|. controlMask , xK_n          ) , submap . M.fromList $
        [ ((modm             , k             ) ,
                do
                spawn ("date>>"++ myNotesPath ++ r) -- Append time
                appendFilePrompt myXPConfig $ myNotesPath ++ r) -- Lprompt
            | ( k, r ) <- zip [xK_w, xK_i, xK_s] ["work", "ideas", "sexy"]
        ])
    ]
    ++
    [((m .|. modm, k), windows $ f i )
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] -- mod-[1..9], Switch to workspace N
        , (f, m) <- [(W.greedyView, 0), (W.shift, mod3Mask), (copy, shiftMask)]
    ]  -- mod-mod3Mask-[1..9], Move client to workspace N
     ++
    [
    ((mod4Mask, xK_o), nextScreen)
    , ((mod4Mask .|. shiftMask, xK_o), shiftNextScreen)
    , ((mod4Mask .|. mod1Mask, xK_o), swapNextScreen)
    ]
    {- | ++
    --mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3, mod-mod3Mask-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
         | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
         , (f, m) <- [(W.view, 0), (W.shift, mod3Mask )]]-}
-- }}}
-- Mouse bindings {{{
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- floating mode and move by dragging
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster)) -- Raise the window to the top of the stack
    , ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w >> windows W.shiftMaster)) -- floating mode and resize by dragging
    -- mouse scroll wheel (button4 and button5)
    , ((modm, button4), (\w -> focus w >> windows W.swapUp))
    , ((modm, button5), (\w -> focus w >> windows W.swapDown))
    ]
-- }}}
-- Layouts: {{{
myLayout = -- spacing 2                                    $  
           avoidStruts                                  $
           onWorkspace (myWorkspaces !! 4) pidginLayout $
           onWorkspace (myWorkspaces !! 6) gimpLayouts  $
           myLayouts
               where
                    myLayouts    = mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $
                                   ( tiled ||| Mirror tiled ||| Circle ||| full )
                    pidginLayout = mkToggle (single REFLECTX) $ withIM (15/100) (Role "buddy_list") tiled
                    gimpLayouts  = gimpLayout ||| gimpLayout2
                    gimpLayout   = mkToggle (single REFLECTX) $ withIM (0.13) (Role "gimp-toolbox") $ reflectHoriz $
                                   withIM (0.17) (Role "gimp-dock") Full
                    gimpLayout2  = mkToggle (single REFLECTX) $ withIM (0.13) (Role "gimp-toolbox") $
                                   withIM (0.17) (Role "gimp-dock") Full
                    tiled        = smartBorders (ResizableTall nmaster delta ratio [])
                    full         = noBorders Full
                    nmaster      = 1
                    delta        = 3/100
                    ratio        = toRational goldenRatio
                    goldenRatio  = 2/(1+sqrt(5)::Double);
-- }}}
-- Window rules: {{{
fullhookName :: String
fullhookName = "__fullfloat"
-- ToggleNextFullFloat 
fullFloatNext :: ManageHook
fullFloatNext = toggleHook fullhookName doFullFloat

-- | @floatNext True@ arranges for the next spawned window to be
-- sent to the floating layer, @floatNext False@ cancels it.
fullNext :: Bool -> X ()
fullNext = hookNext fullhookName

toggleFullNext :: X ()
toggleFullNext = toggleHookNext fullhookName 

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
    , [title =? x --> doFullFloat | x <- myFullFloats]
    , [isDialog    --> doFloat     ]
    , [isFullscreen --> doFullFloat]
    ]
        where
            doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
            myTFloats = ["Downloads", "XCalc", "Xmessage","Save As..."]
            myFullFloats = ["freshwrapper fullscreen window"]
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

myPlacement = withGaps (16,0,16,0) (smart (0.5,0.5)) -- Better floating windows
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

-- myFadeHookHack = ( =<< ) idHook $ liftX (fadeWindowsLogHook myFadeHook)
-- }}}
-- debugcommands {{{
commands :: X [(String, X ())]
commands = return $ myCommands
            where
                myCommands=[("killall" , killAll)
                           ,("tagmocp" , withFocused (addTag "mocp"))
                           ]
-- }}}
-- XPropManage {{{
xPropMatches :: [XPropMatch]
xPropMatches = [ ([ (wM_COMMAND, any ("mocp" `isInfixOf`))], pmX (addTag "mocp") )
                    -- (\w -> addTag "mocp" w >> return (W.shift (myWorkspaces!!3))))
               ]
-- }}}
-- Startup hook {{{
myStartupHook :: X ()
myStartupHook = do
                {-spawnOnce   "gnome-settings-daemon"-}
                {-spawnOnce   "nm-applet"-}
                spawnOnce   "feh --recursive --randomize --bg-scale ~/Pictures/wallpaper/"
                spawnOnce   "compton --backend glx"
                {- spawnOnce "compton -fF -I 0.025 -O 0.065 -D 1 -m 0.8 -i 0.6 -e 0.6"-}
                addScreenCorners [(SCUpperRight,nextWS) , (SCUpperLeft, prevWS)]
-- }}}
-- Event handling {{{
myEventHook = fullscreenEventHook <+> docksEventHook <+> screenCornerEventHook
-- }}}
-- Status bars and logging {{{
logHook' h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }
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
        handleEventHook    = myEventHook, -- <+> debugKeyEvents,
        layoutHook         = myLayout,
        startupHook        = myStartupHook,
        logHook            = logHook' myStatusBarPipe,
        manageHook         = {-- placeHook myPlacement <+> --} manageDocks <+> fullFloatNext <+> myManageHook <+> xPropManageHook xPropMatches  -- <+> myFadeHookHack
        }
-- }}}
