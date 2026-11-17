require('sontek.lsp.mason')
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
  keymap('n', '<leader>gf', vim.lsp.buf.format, bufopts)
  keymap('n', '<leader>ge', '<cmd>lua vim.diagnostic.open_float()<CR>', bufopts)
  keymap('n', '<leader>gq', '<cmd>lua vim.diagnostic.setloclist()<CR>', bufopts)
  keymap('n', 'gn', function() vim.diagnostic.jump({ count = -1, float = true }) end, bufopts)
  keymap('n', 'gm', function() vim.diagnostic.jump({ count = 1, float = true }) end, bufopts)
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
    'tflint',
    'ts_ls'
}

-- Mason builds some servers with a language toolchain. Only auto-install
-- those when the toolchain is on PATH, so startup stays quiet without it.
local requires_toolchain = {
    cssls = 'npm',
    gopls = 'go',
    jsonls = 'npm',
    pyright = 'npm',
    ts_ls = 'npm',
}
local installable = vim.tbl_filter(function(lsp)
    local tool = requires_toolchain[lsp]
    return tool == nil or vim.fn.executable(tool) == 1
end, servers)

require("mason-lspconfig").setup {
    ensure_installed = installable,
}

-- Defaults shared by every server.
vim.lsp.config('*', {
    on_attach = on_attach,
    capabilities = require('cmp_nvim_lsp').default_capabilities(),
})

for _, lsp in pairs(servers) do
    -- Load server settings from the settings file in lsp/servers/
    local has_custom_opts, server_options = pcall(
        require,
        "sontek.lsp.servers." .. lsp
    )
    if has_custom_opts then
        vim.lsp.config(lsp, server_options)
    end
end

-- Enabling a server that isn't installed errors each time a matching
-- file opens. Only enable servers Mason installed or that are on PATH.
local registry = require('mason-registry')
local to_package = require('mason-lspconfig').get_mappings().lspconfig_to_package
local available = vim.tbl_filter(function(lsp)
    local package = to_package[lsp]
    if package and registry.is_installed(package) then
        return true
    end
    local cmd = vim.lsp.config[lsp].cmd
    return type(cmd) == 'table' and vim.fn.executable(cmd[1]) == 1
end, servers)

vim.lsp.enable(available)
