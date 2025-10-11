# tree-sitter-crystal

[Crystal](https://crystal-lang.org/) grammar for [tree-sitter](https://github.com/tree-sitter/tree-sitter).

This grammar is mostly complete, and can parse the majority of Crystal's own source code without errors.

## Setup

Hopefully support for this parser will be upstreamed by editors soon. At the moment, it must be integrated manually.

### Neovim

0. Set up the [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter/tree/main) plugin.

#### For nvim-treesitter v0.10 or newer (recommended)

1. Include this lua snippet in your neovim setup:

   ```lua
   vim.api.nvim_create_autocmd("User", {
     pattern = 'TSUpdate',
     callback = function()
       require('nvim-treesitter.parsers').crystal = {
         install_info = {
           url = 'https://github.com/crystal-lang-tools/tree-sitter-crystal',
           -- path = '<ts-crystal-install-path>', -- if you want to use a local version instead
           generate = false,
           generate_from_json = false,
           queries = 'queries/nvim'
         },
       }
     end,
   })

   vim.treesitter.language.register("crystal", { "cr" })
   ```

2. Restart neovim and run `:TSUpdate`

To confirm the grammar is functioning, run `:checkhealth nvim-treesitter` and look for this line:

```
- crystal             ✓ . ✓ . ✓
```


#### For older nvim-treesitter versions (aka the master branch)

1. Check out this repo locally. Its location will be referred to as `<ts-crystal-install-path>`.
2. In a terminal, navigate to wherever nvim-treesitter is installed (this will depend on your plugin manager). Run:

   ```bash
   mkdir queries/crystal
   cd queries/crystal
   ln -s <ts-crystal-install-path>/queries/nvim/*.scm .
   ```

3. Include this lua snippet in your neovim setup:

   ```lua
   local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
   parser_config.crystal = {
     install_info = {
       url = "<ts-crystal-install-path>",
       files = {"src/parser.c", "src/scanner.c"},
       branch = "main",
     },
     filetype = "cr",
   }
   ```

4. Restart neovim and run `:TSInstall crystal`
