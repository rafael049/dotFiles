import Graphics.X11.ExtraTypes.XF86
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import System.Exit
import System.IO
import XMonad hiding ( (|||) )
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.SimplestFloat
import XMonad.Layout.TrackFloating
import XMonad.ManageHook
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run(spawnPipe, safeSpawn)

-- Config files
xmobarrc = "/home/rafael049/.config/xmobar/xmobarrc.hs"

-- Applications
launcher = "/usr/bin/rofi -show run"
browser  = "brave"
myTerminal = "alacritty"
myFileManager = "ranger"
myGUIFileManager = "pcmanfm"
myMusicPlayer = "LD_PRELOAD=/usr/lib/libcurl.so.4:/home/rafael049/.opt/spotifywm/spotifywm.so /usr/bin/spotify"


-- Scratchpads
scratchpads =
    [ NS "ghci_pad" (myTerminal ++ " -t ghci -e ghci ") (title =? "ghci") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3) )
    , NS "terminal_scratchpad" (myTerminal ++ " -t terminal_scratchpad") (title =? "terminal_scratchpad") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3) )
    , NS "music"  (myMusicPlayer) (className =? "Spotify")  (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3) )
    , NS "ranger_sp" (myTerminal ++ " -t ranger_sp -e ranger ") (title =? "ranger_sp") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3) )
    , NS "pcmanfm_sp" (myGUIFileManager) (className =? "Pcmanfm") (customFloating $ W.RationalRect (0/6) (1/6) (2/3) (2/3) )
    ]

-- Apparence
myBorderWith  = 2
myFocusedBorderColor = "#e658fd"
myNormalBorderColor  = "#191d2e"

myXmobarBackgroundColor       = "#292d3e"
myXmobarWindowTitleColor      = "#f07d75"
myXmobarCurrentWorkspaceColor = "#fdc526"

-- Navigation
myNavigation2DConfig = 
    def { layoutNavigation   = [("Spacing BSP", centerNavigation)]
        , unmappedWindowRect = [("Spacing BSP", singleWindowRect)]
        }

-- Layouts
myLayout = 
    let spacing        = spacingRaw True (Border 0 0 0 0) True (Border 4 4 4 4) True
        resizableTall  = spacing $ ResizableTall 1 (3/100) (1/2) []
        mResizableTall = spacing $ Mirror $ ResizableTall 1 (3/100) (1/2) []
        floating       = trackFloating $ simplestFloat

     in smartBorders $ avoidStruts (
        resizableTall 
        ||| mResizableTall 
        ||| Full 
        ||| floating
    )

myManageHook =
    namedScratchpadManageHook scratchpads <+>
    composeAll [ className =? "mpv"          --> doFloat
               , className =? "GNU Octave"   --> doFloat
               , className =? "Spotify"      --> doFloat
               , className =? "Gimp"         --> doFloat
               , className =? "pavucontrol"  --> doFloat
               , className =? "trabalho_1"   --> doFloat
               , className =? "OpenGL 2D"    --> doFloat
               ]

-- Event Hook
myHandleEventHook = fullscreenEventHook

-- Log Hook (Polybar only)
myLogHook = do
    winset <- gets windowset
    title <- maybe (return "")  (fmap (take 80 . show) . getName) . W.peek $ winset
    let currWs = W.currentTag winset
    let wss = map W.tag $ W.workspaces winset
    let wsStr = join $ map (fmt currWs) $ sort' wss

    io $ appendFile "/tmp/.xmonad-title-log" (colorize myXmobarWindowTitleColor title ++  "\n")
    io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

    where colorize c str =
            "%{F" ++ c ++ "}" ++ str ++ "%{F-}"
          fmt currWs ws
            | currWs == ws = colorize myXmobarCurrentWorkspaceColor $ "[" ++ ws  ++ "]"
            | otherwise   = " " ++ ws ++ " "
          sort' = sortBy (compare `on` (!! 0))


myKeys conf@( XConfig {XMonad.modMask = modMask}) = M.fromList $
    [
    --------------------------
    -- Application bindings --
    --------------------------
     -- Terminal
     ((modMask , xK_Return),
     spawn $ XMonad.terminal conf)

     -- App launcher
    ,((modMask, xK_d),
     spawn launcher)

     -- Browser
    ,((modMask, xK_b),
     spawn browser)

     -- File Manager
    ,((modMask, xK_z),
     spawn $ myTerminal ++ " -e " ++ myFileManager )

     -- Screen Print
    ,((modMask, xK_Print),
     spawn "~/.scripts/screenshot.sh" )

     -- Open XMonad configuration
    ,((modMask .|. shiftMask, xK_F1),
     spawn $ myTerminal ++ " -e vim ~/.xmonad/xmonad.hs")

     -- Open NeoVim configuration
    ,((modMask .|. shiftMask, xK_F2),
     spawn $ myTerminal ++ " -e vim ~/.config/nvim/init.vim")

    --------------------------
    -- Scratchpads bindings --
    --------------------------
    ,((modMask, xK_x),
    namedScratchpadAction scratchpads "terminal_scratchpad")

    ,((modMask .|. shiftMask, xK_x),
    namedScratchpadAction scratchpads "ghci_pad")

    ,((modMask, xK_m),
    namedScratchpadAction scratchpads "music")

    ,((modMask, xK_z),
    namedScratchpadAction scratchpads "ranger_sp")

    ,((modMask .|. shiftMask, xK_z),
    namedScratchpadAction scratchpads "pcmanfm_sp")
    ------------------------
    -- Media key bindings --
    ------------------------

     -- Mute volume.
    ,((0, xF86XK_AudioMute),
     spawn "amixer -q set Master toggle")

     -- Decrease volume.
    ,((0, xF86XK_AudioLowerVolume),
     spawn "amixer -q set Master 5%-")

     -- Increase volume.
    ,((0, xF86XK_AudioRaiseVolume),
      spawn "amixer -q set Master 5%+")

     -- Mute volume.
    ,((modMask .|. controlMask, xK_m),
     spawn "amixer -q set Master toggle")

     -- Next track
    ,((0, xF86XK_AudioNext),
      spawn "playerctl next")
    ,((shiftMask, xF86XK_AudioPlay),
      spawn "playerctl next")

     -- Previous track
    ,((0, xF86XK_AudioPrev),
      spawn "playerctl previous")

     -- Pause/Play track
    ,((0, xF86XK_AudioPlay),
      spawn "playerctl play-pause")


    -------------------------
    -- Layout key bindings --
    -------------------------

     -- Close focused window.
    ,((modMask .|. shiftMask, xK_q),
     kill)

     --  Put back in tiling
    ,((modMask .|. shiftMask, xK_space),
     withFocused $ windows . W.sink)

     -- Resize viewed windows to the correct size.
    ,((modMask .|. shiftMask, xK_n),
     refresh)

     -- Move focus to the next window.
    ,((modMask, xK_Tab),
     windows W.focusDown)

     -- Cycle through the available layout algorithms.
    ,((modMask, xK_space),
     sendMessage NextLayout)

    -- Jump to Layouts
    ,((modMask, xK_u),
     sendMessage $ JumpToLayout "Full" )
    ,((modMask, xK_i),
     sendMessage $ JumpToLayout "Spacing ResizableTall" )
    ,((modMask, xK_o),
     sendMessage $ JumpToLayout "Spacing Mirror ResizableTall" )
    ,((modMask, xK_p),
     sendMessage $ JumpToLayout "SimplestFloat" )

    -- Toggle Struts
    ,((modMask .|. controlMask, xK_F10),
     sendMessage ToggleStruts)

    {-- Standard Xmonad navigation

     -- Move focus to the next window.
    ,((modMask, xK_j),
     windows W.focusDown)

     -- Move focus to the previous window.
    ,((modMask, xK_k),
     windows W.focusUp  )

     -- Move focus to the master window.
    ,((modMask, xK_m),
     windows W.focusMaster  )

     -- Swap the focused window and the master window.
    ,((modMask .|. shiftMask, xK_Return),
     windows W.swapMaster)

     -- Swap the focused window with the next window.
    ,((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

     -- Swap the focused window with the previous window.
    ,((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )
     --}


     -- Increment the number of windows in the master area.
    ,((modMask, xK_comma),
         sendMessage (IncMasterN 1))

     -- Decrement the number of windows in the master area.
    ,((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

     ------------------------------
     -- Navigation2D Key bindings--
     ------------------------------

     -- Switch between layers
     , ((modMask .|. controlMask, xK_space), switchLayer)

     -- Directional navigation of windows
     , ((modMask,                 xK_l), windowGo R True)
     , ((modMask,                 xK_h), windowGo L True)
     , ((modMask,                 xK_k), windowGo U True)
     , ((modMask,                 xK_j), windowGo D True)

     -- Swap adjacent windows
     , ((modMask .|. shiftMask, xK_l), windowSwap R False)
     , ((modMask .|. shiftMask, xK_h), windowSwap L False)
     , ((modMask .|. shiftMask, xK_k), windowSwap U False)
     , ((modMask .|. shiftMask, xK_j), windowSwap D False)

     -- Directional navigation of screens
     --, ((modMask,                 xK_r    ), screenGo R False)
     --, ((modMask,                 xK_l    ), screenGo L False)
     --, ((modMask,                 xK_u    ), screenGo U False)
     --, ((modMask,                 xK_d    ), screenGo D False)

     -- Swap workspaces on adjacent screens
     --, ((modMask .|. controlMask, xK_r    ), screenSwap R False)
     --, ((modMask .|. controlMask, xK_l    ), screenSwap L False)
     --, ((modMask .|. controlMask, xK_u    ), screenSwap U False)
     --, ((modMask .|. controlMask, xK_d    ), screenSwap D False)

     -- Send window to adjacent screen
     --, ((modMask .|. mod1Mask,    xK_r    ), windowToScreen R False)
     --, ((modMask .|. mod1Mask,    xK_l    ), windowToScreen L False)
     --, ((modMask .|. mod1Mask,    xK_u    ), windowToScreen U False)
     --, ((modMask .|. mod1Mask,    xK_d    ), windowToScreen D False)

     -----------------------------
     -- BSP layout key bindings --
     -----------------------------
    --,((modMask .|. controlMask,               xK_l), sendMessage $ ExpandTowards R)
    --,((modMask .|. controlMask,               xK_h), sendMessage $ ExpandTowards L)
    --,((modMask .|. controlMask,               xK_j), sendMessage $ ExpandTowards D)
    --,((modMask .|. controlMask,               xK_k), sendMessage $ ExpandTowards U)
    --,((modMask .|. shiftMask .|. controlMask , xK_l), sendMessage $ ShrinkFrom R)
    --,((modMask .|. shiftMask .|. controlMask , xK_h), sendMessage $ ShrinkFrom L)
    --,((modMask .|. shiftMask .|. controlMask , xK_j), sendMessage $ ShrinkFrom D)
    --,((modMask .|. shiftMask .|. controlMask , xK_k), sendMessage $ ShrinkFrom U)
    --,((modMask,                           xK_g), sendMessage Rotate)
    --,((modMask,                           xK_f), sendMessage Swap)
    --,((modMask,                           xK_n), sendMessage FocusParent)
    --,((modm .|. ctrlMask,              xK_n), sendMessage SelectNode)
    --,((modm .|. shiftMask,             xK_n), sendMessage MoveNode)

    --------------------------------
    -- ResizableTall Key Bindings --
    --------------------------------
    , ((modMask .|. controlMask, xK_j), sendMessage MirrorShrink)
    , ((modMask .|. controlMask, xK_k), sendMessage MirrorExpand)
    , ((modMask .|. controlMask, xK_l), sendMessage Expand)
    , ((modMask .|. controlMask, xK_h), sendMessage Shrink)

    --------------------------------
    -- Spacing Key Bindings --
    --------------------------------
    , ((modMask .|. controlMask, xK_F11), sendMessage $  ModifyWindowBorderEnabled (\x -> not x) )

    -------------------------
    -- XMonad key bindings --
    -------------------------

     -- Quit xmonad.
    ,((modMask .|. shiftMask, xK_e),
     io (exitWith ExitSuccess))

     -- Restart xmonad.
    ,((modMask .|. shiftMask, xK_r),
     restart "xmonad" True)
    ]

    ++

     -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

main = do
    --xmproc <- spawnPipe ("/home/rafael049/.local/bin/xmobar " ++ xmobarrc)
    spawn "polybar bar1"

    -- Adjust keyboard
    spawn "setxkbmap -option caps:escape"
    -- Start Compositor
    spawn "picom -c -r 7 -o 0.2 "
    -- Wallpaper
    spawn "~/.fehbg"
    -- Create Named Pipe for polybar
    forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
        safeSpawn "mkfifo" ["/tmp/" ++ file]

    xmonad $ 
        withNavigation2DConfig myNavigation2DConfig $
        docks $
        ewmh $
        def
            { manageHook      = manageDocks <+> myManageHook
            , handleEventHook = myHandleEventHook
            , layoutHook      = myLayout
            , logHook         = myLogHook
                --dynamicLogWithPP xmobarPP
                --                    { ppOutput  = hPutStrLn xmproc
                --                    , ppTitle   = xmobarColor myXmobarWindowTitleColor "" . shorten 100
                --                    , ppCurrent = xmobarColor myXmobarCurrentWorkspaceColor ""
                --                    , ppSep     = "   "
                --                    }
            , modMask    = mod4Mask
            , keys       = myKeys
            , terminal   = myTerminal

            , borderWidth        = myBorderWith
            , focusedBorderColor = myFocusedBorderColor
            , normalBorderColor  = myNormalBorderColor
            }
