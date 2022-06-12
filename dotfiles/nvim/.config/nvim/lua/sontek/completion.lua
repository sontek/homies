local lspkind = require('lspkind')
local luasnip = require("luasnip")
local cmp = require("cmp")

cmp.setup({
    sources = {
        { name = "nvim_lsp" },
        { name = "nvim_lua" },
        { name = "luasnip" },
        { name = "buffer" },
        { name = "path" },
    },
    formatting = {
      format = lspkind.cmp_format({
        with_text = true,
        menu = {
            buffer = "[buf]",
            nvim_lsp = "[LSP]",
            nvim_lua = "[lua]",
            path = "[path]",
            lusasnip = "[snip]",
        },
        -- defines how annotations are shown
        -- default: symbol
        -- options: 'text', 'text_symbol', 'symbol_text', 'symbol'
        mode = 'symbol_text',
        -- prevent the popup from showing more than 50 chars
        maxwidth = 50,
      })
    },
    mapping = {
        -- Enable "super tab" option for luasnip
        -- https://github.com/hrsh7th/nvim-cmp/wiki/Example-mappings#luasnip
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),
  
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { "i", "s" }),
    },
})
