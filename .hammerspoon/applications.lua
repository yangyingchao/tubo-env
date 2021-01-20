
hs.hotkey.bind({"cmd"}, 'return', function ()
                  hs.application.launchOrFocus("/Applications/Alacritty.app")
end)

hs.hotkey.bind({"cmd", "shift"}, 'return', function ()
                  hs.application.launchOrFocus("/Applications/Terminal.app")
end)
