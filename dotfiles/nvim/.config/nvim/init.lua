-- Provide a `keymap()` function everywhere.
keymap = vim.keymap.set

-- use space as a the leader key
-- We do this in `init` because <leader> isn't lazy processed.  If a
-- plugin is using it as part of the keymapping they will use whatever
-- is defined at the time.
vim.keymap.set(
    { 'n', 'v' },
    '<Space>', '<Nop>',
    { silent = true }
)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Load plugins very early on

require('sontek.plugins')
require('sontek.theme')
-- My editor configuration / default vim settings.
require('sontek.settings')
require('sontek.keymaps')

