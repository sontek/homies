require('sontek.lsp.lsp-installer')
local config = {
    -- disable virtual text
    virtual_text = false,
}

vim.diagnostic.config(config)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  -- vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  keymap('n', 'gD', vim.lsp.buf.declaration, bufopts)
  keymap('n', 'gd', vim.lsp.buf.definition, bufopts)
  keymap('n', 'K', vim.lsp.buf.hover, bufopts)
  keymap('n', 'gi', vim.lsp.buf.implementation, bufopts)
  keymap('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  keymap('n', '<leader>wa', vim.lsp.buf.add_workspace_folder)
  keymap('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  keymap('n', '<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  keymap('n', '<leader>D', vim.lsp.buf.type_definition, bufopts)
  keymap('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
  keymap('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
  keymap('n', 'gr', vim.lsp.buf.references, bufopts)
  keymap('n', '<leader>gf', vim.lsp.buf.formatting, bufopts)
  keymap('n', '<leader>ge', '<cmd>lua vim.diagnostic.open_float()<CR>', bufopts)
  keymap('n', '<leader>gq', '<cmd>lua vim.diagnostic.setloclist()<CR>', bufopts)
  keymap('n', 'n', '<cmd>lua vim.diagnostic.goto_prev()<CR>', bufopts)
  keymap('n', 'm', '<cmd>lua vim.diagnostic.goto_next()<CR>', bufopts)
end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = {
    'cssls',
    'gopls',
    'jsonls',
    'pyright',
    'rust_analyzer',
    'terraformls',
    'tsserver'
}
for _, lsp in pairs(servers) do
    local capabilities = require('cmp_nvim_lsp').update_capabilities(
        vim.lsp.protocol.make_client_capabilities()
    )
    local opts = {
      on_attach = on_attach,
      capabilities = capabilities,
    }

    -- Load server settings from the settings file in lsp/servers/
    local has_custom_opts, server_options = pcall(
        require,
        "sontek.lsp.settings." .. lsp
    )
    if has_custom_opts then
        opts = vim.tbl_deep_extend("force", server_options, opts)
    end

    require('lspconfig')[lsp].setup(opts)
end

