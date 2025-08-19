return {
  { "shaunsingh/nord.nvim" },
  {
    "EdenEast/nightfox.nvim",
    opts = {
      terminal_colors = true,
    },
  },
  {
    "Mofiqul/vscode.nvim",
    opts = {
      style = "light",
      terminal_colors = true,
    },
  },

  -- Configure LazyVim to load colorscheme
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "vscode",
    },
  },
}
