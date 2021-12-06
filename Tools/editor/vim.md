[CS 110L: Safety in Systems Programming (reberhardt.com)](https://reberhardt.com/cs110l/spring-2021/handouts/tools-tips/)

```toml
# All SpaceVim option below [option] section
[options]
    colorscheme = "gruvbox"
    colorscheme_bg = "dark"
    enable_guicolors = true
    # Disable statusline separator, if you want to use other value, please
    # install nerd fonts
    statusline_separator = "arrow"
    statusline_iseparator = "arrow"
    buffer_index_type = 4
    enable_tabline_filetype_icon = true
    enable_statusline_mode = true

# Enable autocomplete layer
[[layers]]
  name = 'autocomplete'
  auto_completion_return_key_behavior = "complete"
  auto_completion_tab_key_behavior = "smart"

[[layers]]
  name = 'colorscheme'
  random_theme = true
  frequency = 'hourly'   # change every  hours

[[layers]]
  name = 'fzf'   # cmds starts with <Leader> f

# Enable Language layers
[[layers]]
  name = 'shell'
  default_position = 'top'
  default_width = 30

[[layers]]
  name = 'lang#rust'


[[layers]]
  name = 'lsp'
  filetypes = [
    'rust'
  ]
  [layers.override_cmd]
    rust = ['rls']

```