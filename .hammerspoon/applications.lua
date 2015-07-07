
hs.hotkey.bind({"cmd"}, 'return', function ()
                  hs.application.launchOrFocus("/Applications/iTerm.app")
end)

hs.hotkey.bind({"cmd", "ctrl"}, 'return', function ()
                  hs.application.launchOrFocus("/Applications/Alacritty.app")
end)
