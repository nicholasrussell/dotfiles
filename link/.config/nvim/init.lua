vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.pack.add({
  {
    src = "https://github.com/nvim-treesitter/nvim-treesitter",
    version = "main",
  },
  {
    src = "https://github.com/Mofiqul/vscode.nvim",
  },
  {
    src = "https://github.com/nvim-tree/nvim-web-devicons",
  },
  {
    src = "https://github.com/nvim-lualine/lualine.nvim",
  },
  -- {
  --   src = "https://github.com/sindrets/diffview.nvim",
  -- },
})

require('options')
require('keymaps')
require('autocmds')
require('treesitter')
require('lsp')
require('theme')
