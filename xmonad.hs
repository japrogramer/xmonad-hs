--  vim: set foldmarker={{{,}}} foldlevel=0 foldmethod=marker :
-- imports {{{
import System.Exit
import System.IO
import System.Posix.Unistd

import XMonad
import XMonad.Actions.WindowGo (title, raiseMaybe, runOrRaise) --, (=?))
import XMonad.Actions.CycleWS
import XMonad.Actions.Search
import XMonad.Actions.Warp
import XMonad.Actions.GridSelect -- grid selector test

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName    -- needed to work around buggy java
import XMonad.Hooks.EwmhDesktops -- needed to work around buggy java
--import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows

import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Circle
import XMonad.Layout.WindowArranger

import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window
import XMonad.Prompt.Ssh

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.WindowProperties (getProp32s)

import Data.Monoid
import Data.Char
import Control.Monad (liftM2)

import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
-- }}}
------------------------------------------------------------------------
-- Urgency hint options extra {{{
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "184", "-h", "16", "-w", "320", "-ta ", "r", "-expand", "l", "-fg",
              "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""
             ]
    }
-- }}}
------------------------------------------------------------------------
-- Color, font and ico,path definitions: {{{
myBitmapsPath    = "/home/japrogramer/.xmonad/icons/"
myFont           = "-*-terminus-*-*-*-*-12*-*-*-*-*"
myIconDir        = "/home/japrogramer/.xmonad/icons"
myNormalFGColor  = "#A30EFF"
myNormalBGColor  = "#2e3436"
myDzenFGColor    = myNormalFGColor
myDzenBGColor    = myNormalBGColor
myUrgentFGColor  = "#0099ff"
myUrgentBGColor  = "#0077ff"
myIconFGColor    = "#777777"
myIconBGColor    = "#0f0f0f"
mySeperatorColor = "#555555"
-- }}}
------------------------------------------------------------------------
-- normal settings {{{
myNormalBorderColor  =  myNormalBGColor 
myFocusedBorderColor =  myNormalFGColor 

-- The preferred terminal program
myTerminal           = "urxvt"
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse  = True
-- Width of the window border in pixels.
myBorderWidth        = 1
-- }}}
------------------------------------------------------------------------
-- Dzen configs {{{
myDzenEvents    = "-e 'button3=' "
-- dzen general options
myDzenGenOpts   = "-fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' -h '16' " 
-- Status Bar
myWorkspaceBar  = "dzen2 -p -ta l -w 640 "
                   ++ myDzenEvents  ++ myDzenGenOpts
-- Conky Bar
myConkyBar      =  "dzen2 -p -ta r -x 640 -w 640 " ++ myDzenGenOpts
-------------------------------------------------------------------------------
-- Looks --
myDzenPP        = defaultPP {
                          ppSep             = "^bg(" ++ myNormalBGColor ++ ")^r(1,15)^bg()",
                          ppWsSep           = " ",
                          ppCurrent         = wrapFgBg myNormalBGColor myNormalFGColor . pad,
                          ppVisible         = wrapFgBg myNormalBGColor myNormalFGColor  . pad,
                          ppHidden          = wrapBg myNormalBGColor . pad,
                          ppHiddenNoWindows = wrapBg myNormalBGColor,
                          ppUrgent          = wrapFg myUrgentFGColor,
                          ppTitle           = shorten 60 . (\y -> " " ++ wrapFg myNormalFGColor y) . (\x -> (filter (`elem` range ) x)),
                          ppLayout          = dzenColor myNormalFGColor myNormalBGColor .
                                                  (\x -> case x of
                                                      "ResizableTall"        -> wrapBitmap "half.xbm"
                                                      "Mirror ResizableTall" -> wrapBitmap "dish.xbm"
                                                      "Full"                 -> wrapBitmap "full.xbm"
                                                      "Circle"               -> wrapBitmap "scorpio.xbm"
                                                      "IM ReflectX IM Full"  -> wrapBitmap "pacman.xbm"
                                                      "IM Grid"              -> "^p(5)#^p(5)"
                                                      _                      -> pad x
                                                  )
                            }
                                where
                                    wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
                                    wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
                                    wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
                                    wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"
                                    range = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [ ' ' ]
-- }}}
------------------------------------------------------------------------
-- workspaces {{{
myWorkspaces         :: [WorkspaceId]
myWorkspaces         =  clickable . (map dzenEscape) $ ["λ","¥","ψ","δ","Σ","ζ","η","θ","¤"]
                                where clickable l     = [  ws  | (i,ws) <- zip [1..] l, let n = if i == 10 then i else 0 ]
--where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" | (i,ws) <- zip [1..] l, let n = if i == 10 then i else 0 ]
-- }}}
------------------------------------------------------------------------
--myxpconfig {{{
myXPConfig =
    XPC { font              = myFont 
        , bgColor           = myNormalBGColor
        , fgColor           = myUrgentFGColor
        , fgHLight          = "black"
        , bgHLight          = "grey"
        , borderColor       = myNormalBorderColor
        , promptBorderWidth = 1
        , promptKeymap      = defaultXPKeymap
        , completionKey     = xK_Tab
        , position          = Bottom
        , height            = 18
        , historySize       = 25
        , historyFilter     = id
        , defaultText       = []
        , autoComplete      = Just 1 
        , showCompletionOnTab = False
        , searchPredicate   = isPrefixOf
        }
-- }}}
------------------------------------------------------------------------
-- modMask the default windows key" is usually mod4Mask.{{{
myModMask       = mod4Mask
-- }}}
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.{{{
--
argumenu = "-b -nb '#2e3436' -nf '#736AFF' -sb '#A30EFF' -sf '#736AFF' -fn '-*-terminus-*-*-*-*-12*-*-*-*-*'"
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- Lterminal
    [ ((modm .|. mod3Mask    , xK_Return ) , spawn $ XMonad.terminal conf             )
    -- Ldmenu
    , ((modm                 , xK_p      ) , spawn ("dmenu_run " ++ argumenu          )  )
    -- Ldtransperancy
    , ((modm                 , xK_minus  ) , spawn ("transset-df -a --dec .1"         )  )
    -- Ldtransperancy
    , ((modm                 , xK_equal  ) , spawn ("transset-df -a --inc .1"         )  )
    -- Ldtransperancy
    , ((modm                 , xK_0      ) , spawn ("transset-df -a -t "              )  )
    -- LRcomp
    , ((modm                 , xK_F9     ) , spawn ("killall compton;sleep 1;compton" )  )
    -- Ltime
    , ((modm .|. mod3Mask    , xK_t      ) , raiseMaybe (runInTerm "-title tty-clock" "sh -c 'tty-clock -sct'"      ) (title =? "tty-clock" )  )
    -- Lranger
    , ((modm                 , xK_f      ) , raiseMaybe (runInTerm "-title ranger" "sh -c 'ranger'"                 ) (title =? "ranger"    )  )
    -- Lmocp
    , ((modm .|. mod3Mask    , xK_m      ) , raiseMaybe (runInTerm "-title mocp" "sh -c 'mocp -T yellow_red_theme'" ) (title =? "mocp"      )  )
    -- Lgvim
    , ((modm .|. mod3Mask    , xK_e      ) , raiseMaybe (runInTerm "-title gvim" "sh -c 'gvim'"                     ) (title =? "gvim"      )  )
    -- Lelinks
    , ((modm .|. mod3Mask    , xK_o      ) , raiseMaybe (runInTerm "-title elinks" "sh -c 'elinks'"                 ) (title =? "elinks"    )  )
    -- Lirssi
    , ((modm .|. mod3Mask    , xK_i      ) , raiseMaybe (runInTerm "-title irssi" "sh -c 'irssi'"                   ) (title =? "irssi"     )  )
    -- Lfirefox
    , ((modm .|. mod3Mask    , xK_f      ) , spawn "firefox"               )
    -- Lpidgin
    , ((modm .|. mod3Mask    , xK_p      ) , spawn "pidgin"                )
    -- Lnautalius
    , ((modm .|. mod3Mask    , xK_n      ) , spawn "nautilus --no-desktop" )
   -- close focused window
    , ((modm .|. mod3Mask    , xK_c      ) , kill)
    -- prompt
    , ((modm .|. mod3Mask    , xK_g      ) , windowPromptGoto myXPConfig          )
    , ((modm .|. mod3Mask    , xK_b      ) , windowPromptBring myXPConfig         )
    --  Reset the layouts on workspace
    , ((modm .|. mod3Mask    , xK_space  ) , setLayout $ XMonad.layoutHook conf   )
    -- moc controls
    -- XF86AudioNext
    , ((0                    , 0x1008ff17) , spawn "mocp -f")
    -- XF86AudioPrev
    , ((0                    , 0x1008ff16) , spawn "mocp -r")
    -- XF86AudioPlay
    , ((0                    , 0x1008ff14) , spawn "mocp -G")
    -- XF86AudioStop
    , ((0                    , 0x1008ff15) , spawn "")
    -- Rotate Layout Algorithms
    , ((modm                 , xK_space )  , sendMessage NextLayout               )
    -- Display grid select test
    , ((modm                 , xK_g     )  , goToSelected $ gsconfig2 myColorizer )
    -- Display runapps grid test
    --, ((modm                 , xK_s)       , spawnSelected gsconfig1 ["xterm" , "mocp" , "gvim"])
    -- display grid select and go
    --, ((modMask .|. mod3Mask , xK_g)       , gridselectWorkspace myGSConfig W.view)
    -- Resize viewed windows to the correct size
    , ((modm                 , xK_n     )  , refresh               )
    -- Move focus to the next window
    , ((modm                 , xK_Tab   )  , windows W.focusDown   )
    -- Move focus to the next window
    , ((modm                 , xK_j     )  , windows W.focusDown   )
    -- Move focus to the previous window
    , ((modm                 , xK_k     )  , windows W.focusUp     )
    -- Move focus to the master window
    , ((modm                 , xK_m     )  , windows W.focusMaster )
    -- Swap the focused window and the master window
    , ((modm                 , xK_Return)  , windows W.swapMaster  )
    -- Swap the focused window with the next window
    , ((modm .|. mod3Mask    , xK_j     )  , windows W.swapDown    )
    -- Swap the focused window with the previous window
    , ((modm .|. mod3Mask    , xK_k     )  , windows W.swapUp      )
    -- Shrink the master area
    , ((modm                 , xK_h     )  , sendMessage Shrink    )
    -- Expand the master area
    , ((modm                 , xK_l     )  , sendMessage Expand    )
    -- Push window back into tiling
    , ((modm                 , xK_t     )  , withFocused $ windows . W.sink )
    -- Increment the number of windows in the master area
    , ((modm                 , xK_comma )  , sendMessage (IncMasterN 1)     )
    -- Deincrement the number of windows in the master area
    , ((modm                 , xK_period)  , sendMessage (IncMasterN (-1))  )
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ((modm                 , xK_b     )  , sendMessage ToggleStruts)
    -- windowArranger settings
    , ((modm .|. shiftMask                   , xK_o ) , sendMessage  Arrange          )
    , ((modm .|. shiftMask   .|. mod3Mask    , xK_o ) , sendMessage  DeArrange        )
    , ((modm .|. shiftMask                   , xK_h ) , sendMessage (MoveLeft      10 )  )
    , ((modm .|. shiftMask                   , xK_l ) , sendMessage (MoveRight     10 )  )
    , ((modm .|. shiftMask                   , xK_j ) , sendMessage (MoveDown      10 )  )
    , ((modm .|. shiftMask                   , xK_k ) , sendMessage (MoveUp        10 )  )
    , ((modm                 .|. controlMask , xK_h ) , sendMessage (IncreaseLeft  10 )  )
    , ((modm                 .|. controlMask , xK_l ) , sendMessage (IncreaseRight 10 )  )
    , ((modm                 .|. controlMask , xK_j ) , sendMessage (IncreaseDown  10 )  )
    , ((modm                 .|. controlMask , xK_k ) , sendMessage (IncreaseUp    10 )  )
    , ((modm .|. shiftMask   .|. mod3Mask    , xK_h ) , sendMessage (DecreaseLeft  10 )  )
    , ((modm .|. shiftMask   .|. mod3Mask    , xK_l ) , sendMessage (DecreaseRight 10 )  )
    , ((modm .|. shiftMask   .|. mod3Mask    , xK_j ) , sendMessage (DecreaseDown  10 )  )
    , ((modm .|. shiftMask   .|. mod3Mask    , xK_k ) , sendMessage (DecreaseUp    10 )  )
    -- adjust for your monitor
    , ((modm .|. shiftMask   .|. mod3Mask , xK_semicolon ), sendMessage (SetGeometry $ Rectangle 250 200 730 400))
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "killall conky dzen2; xmonad --recompile; xmonad --restart")
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
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events {{{
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- possible bind events to the mouse scroll wheel (button4 and button5)
    ]
-- }}}
------------------------------------------------------------------------
-- grid colors {{{
--gsconfig1 = defaultGSConfig { gs_cellheight = 30, gs_font = "xft:Terminus:pixelsize=12", gs_cellwidth = 100 }
gsconfig2 colorizer = (buildDefaultGSConfig colorizer) {
                                gs_cellheight = 24,
                                gs_font = "xft:Terminus:pixelsize=12",
                                gs_cellpadding = 5 
                                }
-- grid colors
myColorizer = colorRangeFromClassName
                      (0x66,0x66,0x99) -- lowest inactive bg
                      (0x33,0x33,0x99) -- highest inactive bg
                      (0x0D,0x17,0x1A) -- active bg
                      black            -- inactive fg
                      white            -- active fg
                where black = minBound
                      white = maxBound
-- }}}
------------------------------------------------------------------------
-- Layouts: {{{
myLayout = avoidStruts                                   $
           onWorkspace (myWorkspaces !! 6 ) gimpLayout   $
           onWorkspace (myWorkspaces !! 4 ) pidginLayout $
           windowArrangeAll myLayouts
                where
                    myLayouts    = tiled ||| Mirror tiled ||| Circle ||| Full
                    gimpLayout   = withIM (0.13) (Role "gimp-toolbox") $
                                   reflectHoriz                        $
                                   withIM (0.17) (Role "gimp-dock") Full
                    tiled        = smartBorders (ResizableTall nmaster delta ratio [])
                    full         = noBorders Full
                    pidginLayout = withIM (15/100) (Role "buddy_list") Grid

                    nmaster      = 1
                    delta        = 3/100
                    ratio        = toRational goldenRatio
                    goldenRatio  = 2/(1+sqrt(5)::Double);
-- }}}
------------------------------------------------------------------------
-- Window rules: {{{
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
myManageHook = composeAll . concat $
    [ [className =? c --> doFloat       | c <- myCFloats ]
    , [ isFullscreen  --> doFullFloat                    ]
    , [isDialog       --> doFloat                        ]
    , [title     =? t --> doFloat       | t <- myTFloats ]
    , [resource  =? r --> doFloat       | r <- myRFloats ]
    , [resource  =? i --> doIgnore      | i <- myIgnores ]
    , [ ( className =? x <||> title =? x <||> resource =? x ) --> doShift      ( myWorkspaces!!0 ) | x <- my1Shifts]
    , [ ( className =? x <||> title =? x <||> resource =? x ) --> doShift      ( myWorkspaces!!1 ) | x <- my2Shifts]
    , [ ( className =? x <||> title =? x <||> resource =? x ) --> doShift      ( myWorkspaces!!2 ) | x <- my3Shifts]
    , [ ( className =? x <||> title =? x <||> resource =? x ) --> doShift      ( myWorkspaces!!3 ) | x <- my4Shifts]
    , [ ( className =? x <||> title =? x <||> resource =? x ) --> doShiftAndGo ( myWorkspaces!!4 ) | x <- my5Shifts]
    , [ ( className =? x <||> title =? x <||> resource =? x ) --> doShiftAndGo ( myWorkspaces!!5 ) | x <- my6Shifts]
    , [ ( className =? x <||> title =? x <||> resource =? x ) --> doShiftAndGo ( myWorkspaces!!6 ) | x <- my7Shifts]
    , [ ( className =? x <||> title =? x <||> resource =? x ) --> doShiftAndGo ( myWorkspaces!!7 ) | x <- my8Shifts]
    , [ ( className =? x <||> title =? x <||> resource =? x ) --> doShiftAndGo ( myWorkspaces!!8 ) | x <- my9Shifts]
    ]
        where
            doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
            myCFloats = ["Ekiga", "XCalc", "Xmessage", "java-lang-Thread", "LCSMain", "Eclipse", "Ediff"] 
            myTFloats = ["Downloads", "Iceweasel Preferences", "Save As...", "Ediff"]
            myRFloats = []
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
------------------------------------------------------------------------
-- fadehook {{{

myFadeHook = composeAll . concat $
    [ 
      [    isUnfocused                                         --> transparency 0.2                   ]
    , [    isDialog                                            --> transparency 0.1                   ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.0 | x <- myIgnores  ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.1 | x <- my1Opacity ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.2 | x <- my2Opacity ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.3 | x <- my3Opacity ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.4 | x <- my4Opacity ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.5 | x <- my5Opacity ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.6 | x <- my6Opacity ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.7 | x <- my7Opacity ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.8 | x <- my8Opacity ]
    , [  ( className =? x <||> title =? x <||> resource =? x ) --> transparency 0.9 | x <- my9Opacity ]
    ]
        where
            myIgnores  = ["Firefox","Wine"]
            my1Opacity = ["Pidgin"]
            my2Opacity = []
            my3Opacity = []
            my4Opacity = ["Gimp"]
            my5Opacity = []
            my6Opacity = []
            my7Opacity = []
            my8Opacity = []
            my9Opacity = []

--myFadeHook = composeAll . concat $ 
--                [                 --[ isFullscreen --> transparency 0.0                                                                        ] ,
                 --[ isInProperty " WM_WINDOW_ROLE      "  " browser                         " /=? False --> transparency 0.0 ] ,
                 --[ isInProperty " _NET_WM_WINDOW_TYPE "  " _NET_WM_WINDOW_TYPE_DOCK        " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_WINDOW_TYPE "  " _NET_WM_WINDOW_TYPE_MENU        " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_WINDOW_TYPE "  " _NET_WM_WINDOW_TYPE_UTILITY     " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_WINDOW_TYPE "  " _NET_WM_WINDOW_TYPE_SPLASH      " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_WINDOW_TYPE "  " _NET_WM_WINDOW_TYPE_DIALOG      " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_WINDOW_TYPE "  " _NET_WM_WINDOW_TYPE_NORMAL      " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_MODAL             " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_STICKY            " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_MODAL             " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_MAXIMIZED_VERT    " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_MAXIMIZED_HORZ    " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_SHADED            " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_SKIP_TASKBAR      " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_SKIP_PAGER        " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_FULLSCREEN        " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_HIDDEN            " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_ABOVE             " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_BELOW             " --> transparency 0.0           ] ,
                 --[ isInProperty " _NET_WM_STATE       "  " _NET_WM_STATE_DEMANDS_ATTENTION " --> transparency 0.0           ] ,
 --               [ transparency 0.025                                                                                         ]
 --               ]
-- }}}
------------------------------------------------------------------------
-- Event handling {{{
-- Defines a custom handler function for X Events. The function should
-- return (AlL True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook = fullscreenEventHook <+> docksEventHook -- <+> fadeWindowsEventHook
-- }}}
------------------------------------------------------------------------
-- Status bars and logging {{{
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
logHook' h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }
-- }}}
------------------------------------------------------------------------
-- Startup hook {{{
myStartupHook :: X ()
myStartupHook = do
                spawnOnce   " gnome-settings-daemon                                                                            "
                spawnOnce   " nm-applet                                                                                        "
                --spawnOnce " xsetroot -cursor_name plus -solid '#2e3436'                                                      "
                spawnOnce   " compton                                                                                          "
                --spawnOnce   " compton -fF -I 0.025 -O 0.065 -D 1 -m 0.8 -i 0.6 -e 0.6                                          "
                spawnOnce   " xloadimage -onroot -fullscreen ~/Pictures/wallpaper/mono.jpg                                     "
-- }}}
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.{{{
main = do
    myStatusBarPipe <- spawnPipe myWorkspaceBar
    conckyBar <- spawnPipe ( "conky -c ~/.xmonad/conkyfd | " ++ myConkyBar)
    xmonad $ myUrgencyHook $ defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = manageDocks <+> myManageHook <+> ( liftX  (fadeWindowsLogHook myFadeHook) >> idHook ),
        handleEventHook    = myEventHook,
      -- fadeWindowsLogHook myFadeHook <+>
        logHook            = logHook' myStatusBarPipe, -- <+> fadeWindowsLogHook myFadeHook,
        startupHook        = myStartupHook
    }
-- }}}
