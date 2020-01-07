--
-- Notion core configuration file
--


-- 
-- Bindings. This includes global bindings and bindings common to
-- screens and all types of frames only. See modules' configuration 
-- files for other bindings.
--


-- WScreen context bindings
--
-- The bindings in this context are available all the time.
--
-- The variable META should contain a string of the form 'Mod1+'
-- where Mod1 maybe replaced with the modifier you want to use for most
-- of the bindings. Similarly ALTMETA may be redefined to add a 
-- modifier to some of the F-key bindings.

function jump_clientwin_or_open(mplex, text, cmd)
    -- The [1] is for the first entry in the array returned by complete_clientwin
    local cw = mod_query.complete_clientwin(text)[1]
    if  cw ~= nil then
        mod_query.gotoclient_handler(mplex, cw)
    else
        ioncore.exec_on(mplex, cmd)
    end
end


function completor_url(cp, str)
    mod_query.popen_completions(cp, "/home/ahsen/code/web_keyword.py "..string.shell_safe(str))
end

function query_fast(mplex, prog, prompt)
    mod_query.query_execwith(mplex, TR(prompt),
                             nil, -- dflt
                             prog or ":man", 
			     completor_url, -- completor
                             nil, -- context
                             false --[[ enable quoting ]])
end


function jump_frame(xid)
    -- iterate until internal fn returns false
    cwin = nil
    ioncore.clientwin_i(
        function(w)
            if w:xid() == xid then
                cwin = w
                return false
            else
                return true
            end
        end)

    if cwin ~= nil then
        print("Found")
        print(cwin)
        cwin:goto_focus()
    end

end


defbindings("WScreen", {
    submap("Control+O", {
        bdoc("Go to previous active object."),
        kpress("O", "ioncore.goto_previous()"),

        kpress("I", "ioncore.goto_next(_chld, 'right')", 
               "_chld:non-nil"),

        bdoc("Create a new workspace of chosen default type."),
        kpress("E", "ioncore.create_ws(_)"),

        bdoc("Go to next/previous screen on multihead setup."),
        --kpress("J", "WScreen.switch_next(_)"),
	kpress("J", "ioncore.goto_next_screen()"),
    }),

    kpress("Control+Mod1+semicolon", "WScreen.switch_prev(_)"),
    kpress("Control+Mod1+apostrophe", "WScreen.switch_next(_)"),

    bdoc("Switch to n:th object (workspace, full screen client window) "..
         "within current screen."),
    kpress(META.."1", "WScreen.switch_nth(_, 0)"),
    kpress(META.."2", "WScreen.switch_nth(_, 1)"),
    kpress(META.."3", "WScreen.switch_nth(_, 2)"),
    kpress(META.."4", "WScreen.switch_nth(_, 3)"),
    kpress(META.."5", "WScreen.switch_nth(_, 4)"),
    kpress(META.."6", "WScreen.switch_nth(_, 5)"),
    kpress(META.."7", "WScreen.switch_nth(_, 6)"),
    kpress(META.."8", "WScreen.switch_nth(_, 7)"),
    kpress(META.."9", "WScreen.switch_nth(_, 8)"),
    kpress(META.."0", "WScreen.switch_nth(_, 9)"),
    
    bdoc("Switch to next/previous object within current screen."),
    kpress(META.."comma", "WScreen.switch_prev(_)"),
    kpress(META.."period", "WScreen.switch_next(_)"),

    submap(META.."K", {
        bdoc("Go to first region demanding attention or previously active one."),
        kpress("K", "mod_menu.grabmenu(_, _sub, 'focuslist')"),
        -- Alternative without (cyclable) menu
        --kpress("K", "ioncore.goto_activity() or ioncore.goto_previous()"),

        --bdoc("Go to previous active object."),
        --kpress("K", "ioncore.goto_previous()"),
        
        --bdoc("Go to first object on activity/urgency list."),
        --kpress("I", "ioncore.goto_activity()"),
        
        bdoc("Clear all tags."),
        kpress("T", "ioncore.tagged_clear()"),
    }),

    bdoc("Go to n:th screen on multihead setup."),
    kpress(META.."Shift+1", "ioncore.goto_nth_screen(0)"),
    kpress(META.."Q", "ioncore.goto_nth_screen(0)"),
    kpress(META.."Shift+2", "ioncore.goto_nth_screen(1)"),
    ---kpress(META.."W", "ioncore.goto_nth_screen(1)"),
    kpress(META.."Shift+3", "ioncore.goto_nth_screen(2)"),
    kpress(META.."E", "ioncore.goto_nth_screen(2)"),
    
    bdoc("Go to next/previous screen on multihead setup."),
    kpress(META.."Shift+comma", "ioncore.goto_prev_screen()"),
    kpress(META.."I", "ioncore.goto_prev_screen()"),
    kpress(META.."O", "ioncore.goto_next_screen()"),
    kpress(META.."grave", "ioncore.goto_next_screen()"),
    
    bdoc("Create a new workspace of chosen default type."),
    kpress(META.."F9", "ioncore.create_ws(_)"),
    
    bdoc("Display the main menu."),
    kpress(ALTMETA.."F12", "mod_query.query_menu(_, _sub, 'mainmenu', 'Main menu:')"),
    --kpress(ALTMETA.."F12", "mod_menu.menu(_, _sub, 'mainmenu', {big=true})"),
    mpress("Button3", "mod_menu.pmenu(_, _sub, 'mainmenu')"),
    
    bdoc("Display the window list menu."),
    mpress("Button2", "mod_menu.pmenu(_, _sub, 'windowlist')"),

    bdoc("Forward-circulate focus."),
    -- '_chld' used here stands to for an actual child window that may not
    -- be managed by the screen itself, unlike '_sub', that is likely to be
    -- the managing group of that window. The right/left directions are
    -- used instead of next/prev, because they work better in conjunction
    -- with tilings.
    kpress(META.."Tab", "ioncore.goto_next(_chld, 'right')", 
           "_chld:non-nil"),
    submap(META.."K", { 
        bdoc("Backward-circulate focus."),
        kpress("AnyModifier+Tab", "ioncore.goto_next(_chld, 'left')", 
               "_chld:non-nil"),
        
        bdoc("Raise focused object, if possible."),
        kpress("AnyModifier+R", "WRegion.rqorder(_chld, 'front')",
               "_chld:non-nil"),
    }),

})


-- Client window bindings
--
-- These bindings affect client windows directly.

defbindings("WClientWin", {
    submap("Control+O", {
       bdoc("Nudge the client window. This might help with some "..
         "programs' resizing problems."),
       kpress_wait("Shift+L", "WClientWin.nudge(_)"),

       bdoc("Kill client owning the client window."),
       kpress("Shift+W", "WClientWin.kill(_)"),
       
       bdoc("Send next key press to the client window. "..
            "Some programs may not allow this by default."),
       --kpress("Q", "WClientWin.quote_next(_)"),
       --kpress("Q", "ioncore.exec_on(_, string.format('0x%x', _:xid()))"),
       -- "Execute" the name of the current window.
       kpress("Q", "ioncore.exec_on(_, string.format('0x%s', _:get_ident().class))"),
       --kpress("Q", "x=WClientWin.xwin(_)"),
       --kpress("Q", "WClientWin.xwin(_)"),
    }),
})


-- Client window group bindings

defbindings("WGroupCW", {
    submap("Control+O", {
        bdoc("Toggle client window group full-screen mode"),
        kpress_wait("F", "WGroup.set_fullscreen(_, 'toggle')"),
    })
})


-- WMPlex context bindings
--
-- These bindings work in frames and on screens. The innermost of such
-- contexts/objects always gets to handle the key press. 

defbindings("WMPlex", {
    submap("Control+O", {
        bdoc("Close current object."),
        kpress_wait("W", "WRegion.rqclose_propagate(_, _sub)"),
    }),
})

xte_mouse_zero="xte 'mousemove 9999 9999'"
xte_mouse_up="xte 'mouseclick 4'"
xte_mouse_down="xte 'mouseclick 5'"
xte_xf86back="xte 'keyup Alt_L' 'key XF86Back'"

--xte_paste="echo \"str `xclip -o`\" | xte"
--xte_paste="xclip -o | pykey"
--xte_paste="xte 'mouseclick 2'"
xte_paste="paste.sh"

-- Frames for transient windows ignore this bindmap
defbindings("WMPlex.toplevel", {
    submap("Control+O", {
        bdoc("Toggle tag of current object."),
        kpress("T", "WRegion.set_tagged(_sub, 'toggle')", "_sub:non-nil"),

        bdoc("Run a terminal emulator."),
        kpress("Shift+K", "ioncore.exec_on(_, 'exec urxvtc -sr -fn fixed -fg grey -bg black +sb')"),
	kpress("K", "ioncore.exec_on(_, 'kon-big')"),
        kpress("Shift+U", "ioncore.exec_on(_,'exec chromium')"),
        kpress("U", "ioncore.exec_on(_,'exec firefox -newwindow')"),
        kpress("Mod1+U", "ioncore.exec_on(_,'exec firefox -newwindow --private-window')"),
        -- kpress("slash", "query_fast(_, 'g', 'Google:')"),
        kpress("slash", "query_fast(_, '/home/ahsen/code/web_keyword.py execute', 'Magic:')"), 
        kpress("L", "query_fast(_, '/home/ahsen/code/web_keyword.py execute', 'Magic:')"), 
        --        kpress("G", "ioncore.exec_on(_,'exec emacsclient -c --alternate-editor=\"\" -q')"),
        kpress("G", "ioncore.exec_on(_,'exec emacsclient -q -c -e \"(ibuffer)\"')"),

        kpress("Shift+Y", "ioncore.exec_on(_, xte_mouse_zero)"),
        kpress("Y", "ioncore.exec_on(_, xte_paste)"),

        kpress("X", "ioncore.exec_on(_,'xscreensaver-lock')"),
        kpress("Shift+X", "ioncore.exec_on(_,'xscreensaver-toggle')"),
        kpress("B", "ioncore.exec_on(_, 'xset dpms force off ; xscreensaver-lock')"),
        kpress("backslash", "ioncore.exec_on(_, 'exec xcalib -i -a')"),

        bdoc("Query for command line to execute."),
        kpress("C", "mod_query.query_exec(_)"),
        kpress("Shift+C", "ioncore.exec_on(_,'conkeror')"),

        kpress("Mod1+F", "jump_clientwin_or_open(_, 'Facebook', 'firefox https://www.facebook.com')"),
        kpress("Mod1+I", "jump_clientwin_or_open(_, 'Inbox', 'firefox https://mail.google.com')"),
        kpress("Mod1+M", "jump_clientwin_or_open(_, 'Messages', 'firefox https://m.facebook.com/messages')"),
        kpress("Mod1+P", "jump_clientwin_or_open(_, 'ipython', 'kon-big -e ipython')"),
    }),

    kpress("Control+Mod1+bracketright", "ioncore.exec_on(_,'mixer.sh +3')"),
    kpress("Control+Mod1+bracketleft", "ioncore.exec_on(_,'mixer.sh -3')"),


    kpress("XF86MonBrightnessDown", "ioncore.exec_on(_, 'brightness.sh down')"),
    kpress("XF86MonBrightnessUp", "ioncore.exec_on(_, 'brightness.sh up')"),    

    kpress("Mod1+bracketright", "ioncore.exec_on(_,'brightness.sh up')"),
    kpress("Mod1+bracketleft", "ioncore.exec_on(_,'brightness.sh down')"),

    kpress("XF86Display", "ioncore.exec_on(_, 'toggle-xrandr.py')"),

    kpress("Control+Mod1+N", "ioncore.exec_on(_, xte_mouse_down)"),
    kpress("Control+Mod1+P", "ioncore.exec_on(_, xte_mouse_up)"),
    kpress("Mod1+Left", "ioncore.exec_on(_, xte_xf86back)"),
    kpress("Print", "ioncore.exec_on(_,'scrot')"),

    bdoc("Toggle tag of current object."),
    kpress(META.."T", "WRegion.set_tagged(_sub, 'toggle')", "_sub:non-nil"),

    bdoc("Lock screen"),
    kpress(META.."L", "notioncore.exec_on(_, notioncore.lookup_script('notion-lock'))"),

    kpress("Mod1+F4", "query_fast(_, 'g', 'Google:')"),
    kpress("Control+backslash", "query_fast(_, 'g', 'Google:')"),
    
    bdoc("Query for command line to execute."),
    kpress(ALTMETA.."F3", "mod_query.query_exec(_)"),

    bdoc("Query for Lua code to execute."),
    -- As a fail-safe, force Mod1 even if META is defined differently.
    kpress("Mod1+F3", "mod_query.query_lua(_)"),

    bdoc("Query for workspace to go to or create a new one."),
    kpress(ALTMETA.."F9", "mod_query.query_workspace(_)"),
    
    bdoc("Query for a client window to go to."),
    --kpress(META.."G", "mod_query.query_gotoclient(_)"),
    kpress("Control+Mod1+G", "mod_query.query_gotoclient(_)"),

    bdoc("Display context menu."),
    --kpress(META.."M", "mod_menu.menu(_, _sub, 'ctxmenu')"),
    kpress(META.."M", "mod_query.query_menu(_, _sub, 'ctxmenu', 'Context menu:')"),
    
    submap(META.."K", {
        bdoc("Detach (float) or reattach an object to its previous location."),
        -- By using _chld instead of _sub, we can detach/reattach queries
        -- attached to a group. The detach code checks if the parameter 
        -- (_chld) is a group 'bottom' and detaches the whole group in that
        -- case.
        kpress("D", "ioncore.detach(_chld, 'toggle')", "_chld:non-nil"),
    }),
})


-- WFrame context bindings
--
-- These bindings are common to all types of frames. Some additional
-- frame bindings are found in some modules' configuration files.

defbindings("WFrame", {
    submap("Control+O", {
        bdoc("Maximize the frame horizontally/vertically."),
        kpress("H", "WFrame.maximize_horiz(_)"),
        kpress("V", "WFrame.maximize_vert(_)"),
        bdoc("Begin move/resize mode."),
        kpress("R", "WFrame.begin_kbresize(_)"),
    }),

    submap(META.."K", {
        bdoc("Maximize the frame horizontally/vertically."),
        kpress("H", "WFrame.maximize_horiz(_)"),
        kpress("V", "WFrame.maximize_vert(_)"),
    }),
    
    bdoc("Display context menu."),
    mpress("Button3", "mod_menu.pmenu(_, _sub, 'ctxmenu')"),
    
    bdoc("Begin move/resize mode."),
    kpress(META.."R", "WFrame.begin_kbresize(_)"),
    
    bdoc("Switch the frame to display the object indicated by the tab."),
    mclick("Button1@tab", "WFrame.p_switch_tab(_)"),
    mclick("Button2@tab", "WFrame.p_switch_tab(_)"),
    
    bdoc("Resize the frame."),
    mdrag("Button1@border", "WFrame.p_resize(_)"),
    mdrag(META.."Button3", "WFrame.p_resize(_)"),
    
    bdoc("Move the frame."),
    mdrag(META.."Button1", "WFrame.p_move(_)"),
    
    bdoc("Move objects between frames by dragging and dropping the tab."),
    mdrag("Button1@tab", "WFrame.p_tabdrag(_)"),
    mdrag("Button2@tab", "WFrame.p_tabdrag(_)"),
           
    bdoc("Switch to next/previous object within the frame."),
    mclick(META.."Button4", "WFrame.switch_next(_)"), 
    mclick(META.."Button5", "WFrame.switch_prev(_)"),
})

-- Frames for transient windows ignore this bindmap

defbindings("WFrame.toplevel", {
    submap("Control+O", {
        -- Display tab numbers when modifiers are released
        submap_wait("ioncore.tabnum.show(_)"),
        
        bdoc("Switch to n:th object within the frame."),
        kpress("1", "WFrame.switch_nth(_, 0)"),
        kpress("2", "WFrame.switch_nth(_, 1)"),
        kpress("3", "WFrame.switch_nth(_, 2)"),
        kpress("4", "WFrame.switch_nth(_, 3)"),
        kpress("5", "WFrame.switch_nth(_, 4)"),
        kpress("6", "WFrame.switch_nth(_, 5)"),
        kpress("7", "WFrame.switch_nth(_, 6)"),
        kpress("8", "WFrame.switch_nth(_, 7)"),
        kpress("9", "WFrame.switch_nth(_, 8)"),
        
        bdoc("Move current object within the frame left/right."),
        kpress("comma", "WFrame.dec_index(_, _sub)", "_sub:non-nil"),
        kpress("period", "WFrame.inc_index(_, _sub)", "_sub:non-nil"),
               
        bdoc("Maximize the frame horizontally/vertically."),
        kpress("H", "WFrame.maximize_horiz(_)"),
        kpress("V", "WFrame.maximize_vert(_)"),

        bdoc("Attach tagged objects to this frame."),
        kpress("A", "ioncore.tagged_attach(_)"),
    }),

    bdoc("Switch to next/previous object within the frame."),
    kpress("Control+apostrophe", "WFrame.switch_next(_)"),
    kpress("Control+semicolon", "WFrame.switch_prev(_)"),

    bdoc("Query for a client window to attach."),
    kpress(META.."A", "mod_query.query_attachclient(_)"),
    
    submap(META.."K", {
        -- Display tab numbers when modifiers are released
        submap_wait("ioncore.tabnum.show(_)"),
        
        bdoc("Switch to n:th object within the frame."),
        kpress("1", "WFrame.switch_nth(_, 0)"),
        kpress("2", "WFrame.switch_nth(_, 1)"),
        kpress("3", "WFrame.switch_nth(_, 2)"),
        kpress("4", "WFrame.switch_nth(_, 3)"),
        kpress("5", "WFrame.switch_nth(_, 4)"),
        kpress("6", "WFrame.switch_nth(_, 5)"),
        kpress("7", "WFrame.switch_nth(_, 6)"),
        kpress("8", "WFrame.switch_nth(_, 7)"),
        kpress("9", "WFrame.switch_nth(_, 8)"),
        kpress("0", "WFrame.switch_nth(_, 9)"),
        
        bdoc("Switch to next/previous object within the frame."),
        kpress("N", "WFrame.switch_next(_)"),
        kpress("P", "WFrame.switch_prev(_)"),
        
        bdoc("Move current object within the frame left/right."),
        kpress("comma", "WFrame.dec_index(_, _sub)", "_sub:non-nil"),
        kpress("period", "WFrame.inc_index(_, _sub)", "_sub:non-nil"),
               
        bdoc("Maximize the frame horizontally/vertically."),
        kpress("H", "WFrame.maximize_horiz(_)"),
        kpress("V", "WFrame.maximize_vert(_)"),

        bdoc("Attach tagged objects to this frame."),
        kpress("A", "ioncore.tagged_attach(_)"),
    }),
})

-- Bindings for floating frames.

defbindings("WFrame.floating", {
    bdoc("Toggle shade mode"),
    mdblclick("Button1@tab", "WFrame.set_shaded(_, 'toggle')"),
    
    bdoc("Raise the frame."),
    mpress("Button1@tab", "WRegion.rqorder(_, 'front')"),
    mpress("Button1@border", "WRegion.rqorder(_, 'front')"),
    mclick(META.."Button1", "WRegion.rqorder(_, 'front')"),
    
    bdoc("Lower the frame."),
    mclick(META.."Button3", "WRegion.rqorder(_, 'back')"),
    
    bdoc("Move the frame."),
    mdrag("Button1@tab", "WFrame.p_move(_)"),
})


-- WMoveresMode context bindings
-- 
-- These bindings are available keyboard move/resize mode. The mode
-- is activated on frames with the command begin_kbresize (bound to
-- META.."R" above by default).

defbindings("WMoveresMode", {
    bdoc("Cancel the resize mode."),
    kpress("AnyModifier+Escape","WMoveresMode.cancel(_)"),

    bdoc("End the resize mode."),
    kpress("AnyModifier+Return","WMoveresMode.finish(_)"),

    bdoc("Grow in specified direction."),
    kpress("Left",  "WMoveresMode.resize(_, 1, 0, 0, 0)"),
    kpress("Right", "WMoveresMode.resize(_, 0, 1, 0, 0)"),
    kpress("Up",    "WMoveresMode.resize(_, 0, 0, 1, 0)"),
    kpress("Down",  "WMoveresMode.resize(_, 0, 0, 0, 1)"),
    kpress("F",     "WMoveresMode.resize(_, 1, 0, 0, 0)"),
    kpress("B",     "WMoveresMode.resize(_, 0, 1, 0, 0)"),
    kpress("P",     "WMoveresMode.resize(_, 0, 0, 1, 0)"),
    kpress("N",     "WMoveresMode.resize(_, 0, 0, 0, 1)"),
    
    bdoc("Shrink in specified direction."),
    kpress("Shift+Left",  "WMoveresMode.resize(_,-1, 0, 0, 0)"),
    kpress("Shift+Right", "WMoveresMode.resize(_, 0,-1, 0, 0)"),
    kpress("Shift+Up",    "WMoveresMode.resize(_, 0, 0,-1, 0)"),
    kpress("Shift+Down",  "WMoveresMode.resize(_, 0, 0, 0,-1)"),
    kpress("Shift+F",     "WMoveresMode.resize(_,-1, 0, 0, 0)"),
    kpress("Shift+B",     "WMoveresMode.resize(_, 0,-1, 0, 0)"),
    kpress("Shift+P",     "WMoveresMode.resize(_, 0, 0,-1, 0)"),
    kpress("Shift+N",     "WMoveresMode.resize(_, 0, 0, 0,-1)"),
    
    bdoc("Move in specified direction."),
    kpress(META.."Left",  "WMoveresMode.move(_,-1, 0)"),
    kpress(META.."Right", "WMoveresMode.move(_, 1, 0)"),
    kpress(META.."Up",    "WMoveresMode.move(_, 0,-1)"),
    kpress(META.."Down",  "WMoveresMode.move(_, 0, 1)"),
    kpress(META.."F",     "WMoveresMode.move(_,-1, 0)"),
    kpress(META.."B",     "WMoveresMode.move(_, 1, 0)"),
    kpress(META.."P",     "WMoveresMode.move(_, 0,-1)"),
    kpress(META.."N",     "WMoveresMode.move(_, 0, 1)"),
})


--
-- Menu definitions
--


-- Main menu
defmenu("mainmenu", {
    menuentry("Run...",         "mod_query.query_exec(_)"),
    menuentry("Terminal",       "mod_query.exec_on_merr(_, XTERM or 'xterm')"),
    menuentry("Lock screen",    
        "notioncore.exec_on(_, notioncore.lookup_script('notion-lock'))"),
    menuentry("Help",           "mod_query.query_man(_)"),
    menuentry("About Notion",      "mod_query.show_about_ion(_)"),
    submenu("Styles",           "stylemenu"),
    submenu("Session",          "sessionmenu"),
})


-- Session control menu
defmenu("sessionmenu", {
    menuentry("Save",           "ioncore.snapshot()"),
    menuentry("Restart",        "ioncore.restart()"),
    menuentry("Restart TWM",    "ioncore.restart_other('twm')"),
    menuentry("Exit",           "ioncore.shutdown()"),
})


-- Context menu (frame actions etc.)
defctxmenu("WFrame", "Frame", {
    -- Note: this propagates the close to any subwindows; it does not
    -- destroy the frame itself, unless empty. An entry to destroy tiled
    -- frames is configured in cfg_tiling.lua.
    menuentry("Close",          "WRegion.rqclose_propagate(_, _sub)"),
    -- Low-priority entries
    menuentry("Attach tagged", "ioncore.tagged_attach(_)", { priority = 0 }),
    menuentry("Clear tags",    "ioncore.tagged_clear()", { priority = 0 }),
    menuentry("Window info",   "mod_query.show_tree(_, _sub)", { priority = 0 }),
})


-- Context menu for groups (workspaces, client windows)
defctxmenu("WGroup", "Group", {
    menuentry("Toggle tag",     "WRegion.set_tagged(_, 'toggle')"),
    menuentry("De/reattach",    "ioncore.detach(_, 'toggle')"), 
})


-- Context menu for workspaces
defctxmenu("WGroupWS", "Workspace", {
    menuentry("Close",          "WRegion.rqclose(_)"),
    menuentry("Rename",         "mod_query.query_renameworkspace(nil, _)"),
    menuentry("Attach tagged",  "ioncore.tagged_attach(_)"),
})


-- Context menu for client windows
defctxmenu("WClientWin", "Client window", {
    menuentry("Kill",           "WClientWin.kill(_)"),
})

