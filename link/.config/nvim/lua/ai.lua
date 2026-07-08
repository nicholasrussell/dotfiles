-- deps:
require('cmp').setup({})
require('render-markdown').setup({})

-- avante:
require('avante').setup({
  provider = "cursor",
  providers = {
    ollama = {
      model = "gpt-oss:20b",
      is_env_set = require("avante.providers.ollama").check_endpoint_alive,
    }
  },
  acp_providers = {
    ["goose"] = {
      command = "goose",
      args = { "acp" },
    },
    ["cursor"] = {
      command = "agent",
      args = { "acp" },
    },
  }
})
