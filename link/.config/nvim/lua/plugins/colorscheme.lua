return {
  -- add nord
  { "shaunsingh/nord.nvim" },
  --- add nightfox
  {
    "EdenEast/nightfox.nvim",
    opts = {
      terminal_colors = true,
    },
  },

  -- Configure LazyVim to load colorscheme
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "nordfox",
    },
  },
}
