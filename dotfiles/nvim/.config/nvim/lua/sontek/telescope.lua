require('telescope').setup {
    pickers = {
        find_files = {
            hidden = true,
            file_ignore_patterns = {
                "^.git/",
                "%.lock",
            },
        },
        live_grep = {
            file_ignore_patterns = {
                "^.git/",
                "%.lock",
            },
            vimgrep_arguments = {
              "rg",
              "--color=never",
              "--no-heading",
              "--with-filename",
              "--line-number",
              "--column",
              "--smart-case",
              "--hidden",
            }
        }
    },
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

-- Key mappings for telescope
-- Fuzzy search for files in the current working directory
keymap('n', '<leader>ff',
    function()
      require('telescope.builtin').find_files {
          previewer = false,
          -- hidden = true
      }
    end
)
-- Grep for text in the current working directory
keymap(
    'n', '<leader>fg',
    require('telescope.builtin').live_grep
)

-- Fuzzy search the current buffer
keymap(
  'n', '<leader>fb',
  require('telescope.builtin').current_buffer_fuzzy_find
)

-- This fuzzy searches open buffers.  Haven't decided if I want that yet.
-- vim.keymap.set(
--   'n', '<leader><leader>',
--   require('telescope.builtin').buffers
-- )
-- 
