-- https://www.hammerspoon.org/docs/hs.application.html
-- http://www.hammerspoon.org/docs/hs.application.html#frontmostApplication
hs.hotkey.bind({"ctrl"}, "F12", function()
  local app = hs.application.get("kitty")
  -- use bundleID rather than name: https://github.com/kovidgoyal/kitty/issues/45#issuecomment-1575401011
  -- local app = hs.application.get("net.kovidgoyal.kitty")

    if app:isFrontmost() then
        app:hide()
    else
        hs.application.launchOrFocus(app:name())
    end

    -- https://github.com/kovidgoyal/kitty/issues/45#issuecomment-1174835371
    -- If you set macos_hide_from_tasks yes this doesn't work properly
    -- since there is no menu item to select when kitty is in focus.
    -- I would therefore recommend to use the launchOrFocus method
    -- instead
    
  -- if app then
  --     if not app:mainWindow() then
  --         app:selectMenuItem({"kitty", "New OS window"})
  --     elseif app:isFrontmost() then
  --         app:hide()
  --     else
  --         app:activate()
  --     end
  -- else
  --     hs.application.launchOrFocus("kitty")
  --     app = hs.application.get("kitty")
  -- end

  -- app:mainWindow():moveToUnit'[100,50,0,0]'
  -- app:mainWindow().setShadows(false)
end)



-- https://github.com/jeremyf/dotzshrc/blob/main/symlinks/.hammerspoon/init.lua

local meh = {"ctrl", "alt", "cmd"}
local hyper = {"ctrl", "alt", "cmd", "shift"}

-- Optional configuration of beginEditShellCommand
spoon.editWithEmacs.openEditorShellCommand = "EDITOR -e '(hammerspoon-edit-begin)'"

hs.loadSpoon("editWithEmacs")
if spoon.editWithEmacs then
   local bindings = {
      edit_selection =  { hyper, "e"},
      edit_all       = { meh, "e"}
      -- edit_selection =  { {"alt"}, "1"},
      -- edit_all       = { {"alt"}, "2"}
   }   
   spoon.editWithEmacs:bindHotkeys(bindings)
end
