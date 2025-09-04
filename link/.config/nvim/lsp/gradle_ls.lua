--[[
return {
  cmd = { 'gradle-language-server' },
  filetypes = { 'gradle', 'groovy' },
  root_markers = {
    'settings.gradle',
    'build.gradle',
  },
  init_options = {
    settings = {
      gradleWrapperEnabled = true,
    },
  },
}
]]
