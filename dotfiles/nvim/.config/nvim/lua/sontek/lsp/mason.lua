require("mason").setup {
    -- automatically detect which servers to install
    -- based on which servers are set up via lspconfig
    automatic_installation = true,
    ui = {
        icons = {
            server_installed = "✓",
            server_pending = "➜",
            server_uninstalled = "✗"
        }
    }
}

