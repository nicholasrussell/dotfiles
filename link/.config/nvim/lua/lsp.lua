-- # Langs
-- ## bash
vim.lsp.enable('bashls')

-- ## clangd
vim.lsp.enable('clangd')

-- ## clojure
vim.lsp.enable('clojure_lsp')

-- ## lua
vim.lsp.enable('lua_ls')

-- ## rust
vim.lsp.enable('rust_analyzer')

-- ## typescript / javascript
vim.lsp.enable('ts_ls')
-- vim.lsp.enable('tsgo')

-- ## zig
-- vim.lsp.enable('zls')

-- # Autocmds
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('userautocmd_lsp', {}),
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    -- enable auto-complete
    if client:supports_method('textDocument/completion') then
      vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
    end
    -- lint on save
    -- usually not needed if server supports "textDocument/willSaveWaitUntil"
    if not client:supports_method('textDocument/willSaveWaitUntil')
        and client:supports_method('textDocument/formatting') then
      vim.api.nvim_create_autocmd('BufWritePre', {
        group = vim.api.nvim_create_augroup('userautocmd_lsp', { clear = false }),
        buffer = args.buf,
        callback = function()
          vim.lsp.buf.format({ bufnr = args.buf, id = client.id, timeout_ms = 1000 })
        end,
      })
    end
  end
})

-- # Diagnostics
vim.diagnostic.config({
  virtual_lines = true,
  virtual_text = true
})
