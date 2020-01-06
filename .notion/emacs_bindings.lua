-- Authors: Matthieu MOY <Matthieu.Moy@imag.fr>, David Hansen <david.hansen@physik.fu-berlin.de>, Tyranix <tyranix@gmail.com>, Adam Di Carlo <aph@debian.org>
-- License: Public domain
-- Last Changed: 2007-04-27
--
-- Emacs-like keyboard configuration for Ion, version 3.
-- Written by Matthieu MOY (Matthieu.Moy@imag.fr) on February 15, 2005.
-- No copyright.

-- Please note: You will almost certainly want to change Ion3's default
-- META binding to Alt (as this will interfere with Emacs).
-- See the FAQ or the wiki entries on this subject.
-- Use cfg_debian.lua on Debian installations to make the change global
-- across ion3 upgrades.

--[[
  Copy this file in your ~/.ion3/ directory
  Add the line
   dopath("emacs-bindings")
  At the *end* of your ~/.ion3/cfg_ion.lua file, or in ~/.ion3/cfg_user.lua

  These bindings are not for every ion3 function.  By copying the file, you
  will add bindings for ion3.  You will still have to use the default ion3
  bindings for any functions not bound here.

  Comments/Feedback welcome.  See the changelog at the end.
--]]

--[[
List of the bindings.  These are executed after the main ion3 bindings.

What you should do is familiarize yourself with the main ion3 bindings and
then review these additions.  The Emacs bindings should provide additional
bindings *not* replace the default ion3 bindings.

Note that ion bindings use "META+<something>" but since we're all Emacs
users here, I left it in Emacs form "META-<something>".

  Ion binding     Emacs binding  Description

WTiling:
  META-x 0        C-x 0          Destroy the current frame
  META-x 1        C-x 1          Move all windows to single frame; destroy rest
  META-x 2        C-x 2          Split current frame vertically
  META-x 3        C-x 3          Split current frame horizontally
  META-x META-x 2                Split current floating frame vertically
  META-x META-x 3                Split current floating frame horizontally
  META-x o        C-x o          Cycle to the next workspace

 WMPlex:
  META-x b        C-x b          Query client win to attach to active frame
  META-x k        C-x k          Close current object
  META-u META-x k                Kill client owning current client window
  META-q          C-q            Enter next character literally
  META-Ctrl-Space                Attach tagged objects to this frame
  META-Shift-:    C-:            Query for Lua code to execute

 WScreen
  META-Shift-:    C-:            Query for Lua code to execute (XXX DUP?)
  META-x b        C-x b          Query for a client window to go to
  META-x META-b   C-x C-b        Display the window list menu
  META-x w                       Query for workspace to go to (or create)
  META-x META-w                  Display list of workspaces

 WEdln:
  M-f             M-f            Skip one word forward
  M-<right>       M-<right>      Skip one word forward
  M-b             M-b            Skip one word backward
  M-<left>        M-<left>       Skip one word backward
  M-d             M-d            Delete one word forward
  M-<backspace>   M-<backspace>  Delete one word backward
  C-k             C-k            Delete from current position to end of line
  C-w             C-w            Cut selection
  C-y             C-y            Paste from the clipboard
  C-<space>       C-SPC          Set mark/begin selection
  C-g             C-g            Clear mark/cancel selection or abort
  M-p             M-p            Select next (matching) history entry
  M-n             M-n            Select previous (matching) history entry

TODO:
  Global M-h k		  C-h k		 Describe key
  WEdlin M-t		  M-t		 Transpose word
--]]

Emacs = {};

--
-- WEdln
--
-- odds:
--   M-d, M-backspace don't copy to the clipboard
--   M-p, M-n do eshell like history, C-p, C-n model-line like history
--

function WEdln:cut_to_eol ()
    self:set_mark()
    self:eol()
    self:cut()
end

function WEdln:clear_mark_or_abort ()
    if -1 ~= self:mark() then
        self:clear_mark()
    else
        self:cancel()
    end
end

Emacs.WEdln = {
    bdoc("Skip one word forward."),
    kpress("Mod1+f", "WEdln.skip_word(_)"),

    bdoc("Skip one word backward."),
    kpress("Mod1+Right", "WEdln.skip_word(_)"),

    bdoc("Skip one word backward."),
    kpress("Mod1+b", "WEdln.bskip_word(_)"),

    bdoc("Skip one word backward."),
    kpress("Mod1+Left", "WEdln.bskip_word(_)"),

    bdoc("Delete one word forward."),
    kpress("Mod1+d", "WEdln.kill_word(_)"),

    bdoc("Delete one word backward."),
    kpress("Mod1+BackSpace", "WEdln.bkill_word(_)"),

    bdoc("Delete from current position to the end of the line."),
    kpress("Control+K", "WEdln.cut_to_eol(_)"),

    bdoc("Cut selection."),
    kpress("Control+W", "WEdln.cut(_)"),

    bdoc("Copy selection."),
    kpress("Mod1+W", "WEdln.copy(_)"),

    bdoc("Paste from the clipboard."),
    kpress("Control+Y", "WEdln.paste(_)"),

    bdoc("Set mark/begin selection."),
    kpress("Control+space", "WEdln.set_mark(_)"),

    bdoc("Clear mark/cancel selection or abort."),
    kpress("Control+G", "WEdln.clear_mark_or_abort(_)"),

    bdoc("Select next (matching) history entry."),
    kpress("Mod1+p", "WEdln.history_prev(_, true)"),

    bdoc("Select previous (matching) history entry."),
    kpress("Mod1+n", "WEdln.history_next(_, true)"),

    bdoc("Try to complete the entered text or cycle through completions."),
    kpress("Control+I", "WEdln.complete(_, 'next', 'normal')"),
}

defbindings("WEdln", Emacs.WEdln)
