require 'nvim-treesitter'.setup {}
require 'nvim-treesitter'.install {
  'bash',
  'c',
  'clojure',
  'css',
  'dockerfile',
  'editorconfig',
  'haskell',
  'html',
  'java',
  'javascript',
  'json',
  'lua',
  'make',
  'markdown',
  'markdown_inline',
  'ocaml',
  'python',
  'rust',
  'scheme',
  'sql',
  'tmux',
  'toml',
  'tsx',
  'typescript',
  'xml',
  'yaml',
  'zig'
}

local filetype_table = {
  sh = 'bash',
  c = 'c',
  cpp = 'c',
  clj = 'clojure',
  css = 'css',
  docker = 'dockerfile',
  hs = 'haskell',
  lhs = 'haskell',
  html = 'html',
  java = 'javascript',
  json = 'json',
  lua = 'lua',
  md = 'markdown',
  ocaml = 'ocaml',
  py = 'python',
  rs = 'rust',
  scm = 'scheme',
  sql = 'sql',
  tmux = 'tmux',
  toml = 'toml',
  tsx = 'tsx',
  ts = 'typescript',
  xml = 'xml',
  yml = 'yaml',
  yaml = 'yaml',
  zig = 'zig',
}

local function get_keys(t)
  local keys = {}
  for key, _ in pairs(t) do
    table.insert(keys, key)
  end
  return keys
end


vim.api.nvim_create_autocmd('FileType', {
  pattern = get_keys(filetype_table),
  callback = function(args)
    vim.treesitter.start(args.buf, filetype_table[args.match])
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
  end,
})

-- :TSUpdate
