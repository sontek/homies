import XMonad

main = xmonad defaultConfig
    { modMask = mod4Mask -- user Super instead of Alt
    , terminal = "urxvt"
    }
