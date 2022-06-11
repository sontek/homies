require('telescope').setup {
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  }
}
-- To get fzf loaded and working with telescope, we need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')

-- nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
-- nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
-- nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
-- nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>

-- Key mappings for telescope
-- Fuzzy search for files in the current working directory
vim.keymap.set('n', '<leader>ff',
    function()
      require('telescope.builtin').find_files { previewer = false }
    end
)
-- Grep for text in the current working directory
vim.keymap.set(
    'n', '<leader>fg',
    require('telescope.builtin').live_grep
)

-- Fuzzy search the current buffer
vim.keymap.set(
  'n', '<leader>fb',
  require('telescope.builtin').current_buffer_fuzzy_find
)

-- This fuzzy searches open buffers.  Haven't decided if I want that yet.
-- vim.keymap.set(
--   'n', '<leader><leader>',
--   require('telescope.builtin').buffers
-- )
-- 
