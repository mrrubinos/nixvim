{ inputs, pkgs }:
{
  plugins = {
    barbar = {
      enable = true;
      autoLoad = true;
    };
    bufferline.enable = true;
    cmp = {
      enable = true;
      autoEnableSources = true;
      settings = {
        mapping = {
          "<C-n>" = "cmp.mapping.select_next_item()";
          "<C-p>" = "cmp.mapping.select_prev_item()";
          "<C-d>" = "cmp.mapping.scroll_docs(-4)";
          "<C-f>" = "cmp.mapping.scroll_docs(4)";
          "<C-e>" = "cmp.mapping.close()";
          "<CR>" = "cmp.mapping.confirm({ select = true })";
          "<Tab>" = "cmp.mapping(cmp.mapping.select_next_item(), {'i', 's'})";
          "<S-Tab>" = "cmp.mapping(cmp.mapping.select_prev_item(), {'i', 's'})";
        };
        sources = [
          { name = "nvim_lsp"; }
          { name = "treesitter"; }
          { name = "path"; }
          { name = "buffer"; }
        ];
        snippet.expand = ''function(args) require('luasnip').lsp_expand(args.body) end'';
      };
    };
    cmp_luasnip.enable = true;
    codecompanion = {
      enable = true;
      autoLoad = true;
      settings = {
        adapters = {
          # Anthropic adapter configuration
          anthropic.__raw = ''
            function()
            return require('codecompanion.adapters').extend('anthropic', {
                env = {
                api_key = "cmd:cat /run/agenix/claude-api-key"
                }
                })
          end
            '';

          # Gemini adapter configuration
          gemini.__raw = ''
            function()
            return require('codecompanion.adapters').extend('gemini', {
                env = {
                api_key = "cmd:cat /run/agenix/gemini-api-key"
                }
                })
          end
            '';
        };

        opts = {
          send_code = true;
          use_default_actions = true;
          use_default_prompts = true;
          log_level = "DEBUG"; # As requested for debugging
        };

        strategies = {
          # Set anthropic as default for all interactions
          agent = {
            adapter = "anthropic";
          };
          chat = {
            adapter = "anthropic";
          };
          inline = {
            adapter = "anthropic";
          };
        };
      };
    };
    comment.enable = true;
    # Dashboard configuration
    dashboard = {
      enable = true;
      settings.config = {
        header = [
          "                                            "
          "███╗   ██╗██╗██╗  ██╗██╗   ██╗██╗███╗   ███╗"
          "████╗  ██║██║╚██╗██╔╝██║   ██║██║████╗ ████║"
          "██╔██╗ ██║██║ ╚███╔╝ ██║   ██║██║██╔████╔██║"
          "██║╚██╗██║██║ ██╔██╗ ╚██╗ ██╔╝██║██║╚██╔╝██║"
          "██║ ╚████║██║██╔╝ ██╗ ╚████╔╝ ██║██║ ╚═╝ ██║"
          "╚═╝  ╚═══╝╚═╝╚═╝  ╚═╝  ╚═══╝  ╚═╝╚═╝     ╚═╝"
          "                                            "
          "                                            "
        ];
        shortcut = [
          {
            icon = " ";
            desc = "Find";
            key = "f";
            keymap = "<leader>ff";
            action = "Telescope find_files";
          }
          {
            icon = " ";
            desc = "New";
            key = "n";
            action = "enew";
          }
          {
            icon = " ";
            desc = "Grep";
            key = "/";
            keymap = "<leader>fg";
            action = "Telescope live_grep";
          }
          {
            icon = " ";
            desc = "Log";
            key = "l";
            keymap = "<leader>pl";
            action = "lua require('para').open_log()";
          }
          {
            icon = " ";
            desc = "Tasks";
            key = "t";
            keymap = "<leader>pt";
            action = "lua require('para').open_tasks()";
          }
          {
            icon = " ";
            desc = "Notes";
            key = "s";
            keymap = "<leader>ps";
            action = "lua require('para').search_notes_by_content()";
          }
          {
            icon = " ";
            desc = "Quit";
            key = "q";
            action = "qa";
          }
        ];
      };
    };
    gitsigns.enable = true;
    lightline = {
      enable = true;
      settings = {
        colorscheme = "ayu_dark";
      };
    };
    # LSP configuration
    lsp = {
      enable = true;
      servers = {
        bashls.enable = true;
        elixirls.enable = true;
        erlangls.enable = true;
        marksman.enable = true;
        nil_ls.enable = true;
      };
    };
    lsp-lines = {
      enable = true;
      autoLoad = true;
    };
    luasnip = {
      enable = true;
      autoLoad = true;
    };
    # Neo-tree configuration
    neo-tree = {
      enable = true;
      closeIfLastWindow = true;
      window = {
        position = "left";
        width = 30;
      };
    };
    # Noice configuration
    noice = {
      enable = true;
      settings = {
        cmdline = {
          enabled = true;
          view = "cmdline_popup";
        };
        lsp = {
          hover = {
            enabled = true;
          };
          signature = {
            enabled = true;
          };
        };
      };
    };
    # Telescope configuration
    telescope = {
      enable = true;
    };
    # Treesitter configuration
    treesitter = {
      enable = true;
      settings = {
        auto_install = true;
        ensure_installed = ["bash" "erlang" "elixir" "eex" "heex" "markdown" "markdown_inline" "nix"];
      };
    };
    web-devicons.enable = true;
    # Which-key configuration
    which-key = {
      enable = true;
      settings = {
        spec = [
          {
            __unkeyed-1 = "<leader>p";
            group = "󰂮 PARA System";
          }
          {
            __unkeyed-1 = "<leader>pl";
            desc = "Open log file";
          }
          {
            __unkeyed-1 = "<leader>pla";
            desc = "Add log entry";
          }
          {
            __unkeyed-1 = "<leader>plx";
            desc = "Archive log entries";
          }
          {
            __unkeyed-1 = "<leader>pt";
            desc = "Open tasks file";
          }
          {
            __unkeyed-1 = "<leader>pta";
            desc = "Add task";
          }
          {
            __unkeyed-1 = "<leader>ptx";
            desc = "Archive done tasks";
          }
          {
            __unkeyed-1 = "<leader>pn";
            desc = "Search notes by name";
          }
          {
            __unkeyed-1 = "<leader>ps";
            desc = "Search notes by content";
          }
          {
            __unkeyed-1 = "<leader>px";
            desc = "Archive project";
          }
          {
            __unkeyed-1 = "<leader>tt";
            desc = "Toggle task done/undone";
          }
        ];
      };
    };
  };
  extraPlugins = with pkgs.vimPlugins; [
    lazygit-nvim
    (pkgs.vimUtils.buildVimPlugin {
      pname = "gmn-nvim";
      version = "latest";
      src = inputs.gmn;
      dependencies = with pkgs.vimPlugins; [
        plenary-nvim
      ];
    })
    (pkgs.vimUtils.buildVimPlugin {
      pname = "template";
      version = "latest";
      src = inputs.template;
    })
  ];
  extraConfigLua = ''
    require('gmn').setup({
       configFilepath = '~/.config/gemini.nvim/config.json',
       timeout = 30 * 1000,
       model = 'gemini-2.5-pro-preview-05-06',
       safetyThreshold = 'BLOCK_ONLY_HIGH',
       stripOutermostCodeblock = function()
         return vim.bo.filetype ~= 'markdown'
       end,
       verbose = false
    })
    require('template').setup({
      temp_dir = '~/.local/share/nvim/templates',
      author = 'Miguel Rubinos',
      email = 'miguel@nomasystems.com',
      project = {
        ['erl'] = {
          'application',
          'common_test',
          'escript',
          'gen_event',
          'gen_fsm',
          'gen_server',
          'gen_statem',
          'header',
          'library',
          'supervisor'
        }
      }
    })

    -- PARA Method Task & Note Management Setup
    local para = require('para')
    
    -- Get PKM path from environment variable at runtime, with fallback
    local pkm_path = vim.fn.getenv("PARA_BASE")
    if pkm_path == vim.NIL or pkm_path == "" then
      pkm_path = vim.fn.getenv("NIXVIM_PKM_PATH")
    end
    if pkm_path == vim.NIL or pkm_path == "" then
      pkm_path = "~/Documents/PARA"
    end
    pkm_path = vim.fn.expand(pkm_path)
    
    para.setup({
      -- Base directory for all PARA content (configurable at runtime)
      base_path = pkm_path,
      log_file = pkm_path .. "/log.md",
      log_archive_dir = pkm_path .. "/logs/archive",
      tasks_file = pkm_path .. "/tasks.md",
      tasks_archive_file = pkm_path .. "/tasks_archive.md"
    })
  '';
}
