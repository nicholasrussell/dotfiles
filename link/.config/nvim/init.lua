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
  -- ai deps:
  {
    src = "https://github.com/hrsh7th/nvim-cmp",
  },
  {
    src = "https://github.com/MeanderingProgrammer/render-markdown.nvim",
  },
  {
    src = "https://github.com/nvim-lua/plenary.nvim",
  },
  {
    src = "https://github.com/MunifTanjim/nui.nvim",
  },
  {
    src = "https://github.com/yetone/avante.nvim",
  },
})

require('options')
require('keymaps')
require('autocmds')
require('treesitter')
require('lsp')
require('theme')
require('ai')
