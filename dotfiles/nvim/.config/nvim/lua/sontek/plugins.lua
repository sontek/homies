local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

-- boostrap Packer plugin which we'll use to download
-- all the rest of the plugins we'd like to use.
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({
      'git',
      'clone',
      '--depth',
      '1',
      'https://github.com/wbthomason/packer.nvim',
      install_path
    })
end

-- Re-run PackerCompiler whenever we change the plugins file
local packer_group = vim.api.nvim_create_augroup(
    'Packer',
    {
        clear = true
    }
)
vim.api.nvim_create_autocmd(
    'BufWritePost',
    {
        command = 'source <afile> | PackerCompile',
	group = packer_group,
	pattern = '*/plugins.lua'
    }
)


return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim' -- Package manager
    -- A reasonably decent colorscheme
    use { "ellisonleao/gruvbox.nvim" }

    -- UI to select things (files, grep results, open buffers...)
    -- https://github.com/nvim-telescope/telescope.nvim
    use {
        'nvim-telescope/telescope.nvim',
        requires = {
            'nvim-lua/plenary.nvim'
        }
    }
    -- fzf-native is a faster sorting plugin for working with large projects
    use {
        'nvim-telescope/telescope-fzf-native.nvim',
        run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build'
    }

    -- Very fast syntax highlighting.
    use 'nvim-treesitter/nvim-treesitter'

    -- Automatically set up configuration after cloning packer.nvim
    -- Should remain at the bottom, after all plugins
    if packer_bootstrap then
        require('packer').sync()
    end

	-- load plugin specific settings here
	require('sontek.telescope')
end)

