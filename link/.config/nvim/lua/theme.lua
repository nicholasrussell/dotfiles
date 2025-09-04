require("vscode").setup({
  style = "light",
  transparent = true,
  italic_comments = true,
  italic_inlayhints = true,
  underline_links = true,
  terminal_colors = true,
})
vim.cmd.colorscheme("vscode")

-- lualine
require("lualine").setup({
  options = {
    icons_enabled = true,
    theme = "vscode",
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch" },
    lualine_c = {
      { "diagnostics", symbols = { error = " ", warn = " ", info = " ", hint = " ", }, },
      { "filetype", icon_only = true, separator = "", padding = { left = 1, right = 0 }, },
      { "filename", padding = { left = 0, right = 0 }, },
    },
    lualine_x = { },
    lualine_y = {
      { "progress", separator = " ", padding = { left = 1, right = 0 }, },
      { "location", padding = { left = 0, right = 1 }, },
    },
    lualine_z = { },
  },
})
