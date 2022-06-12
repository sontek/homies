require('nvim-tree').setup {
  view = {
    width = 45,
  }
}

-- Key mappings for nvim-tree
keymap('n', '<leader>n', ":NvimTreeToggle<CR>")
