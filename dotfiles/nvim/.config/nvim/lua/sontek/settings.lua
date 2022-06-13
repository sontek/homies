-- fake it so we kind of look like vimscript.
local set = vim.opt

-- Enable 24-bity RGB colors.
set.termguicolors = true

-- Sets us up so yanks go to the system clipboard
set.clipboard = "unnamedplus"

-- show line numbers
set.number = true

-- Default to 4 spaces with tab.  Don't use tab character.
set.tabstop = 4
set.shiftwidth = 4
set.softtabstop = 4
set.expandtab = true

-- Show a line across the screen for the row the cursor is on
set.cursorline = true

-- always show the sign column so things don't redraw when
-- we have them.
set.signcolumn = 'yes'

-- Show some characters on the screen visibly even though they
-- would usually be hidden
set.list = true
set.listchars = {
    -- eol = '↲',
    tab = '▸ ',
    trail = '·'
}


-- search case insensitive unless we put a capital
-- in the search.
set.ignorecase = true
set.smartcase = true

-- highlight column 80 so we know when we are getting
-- a little long winded. i.e prevent arrow code.
set.colorcolumn = '80'


-- Configure completion to provide better pop-up experience
set.completeopt = { "menu", "menuone", "noselect" }
