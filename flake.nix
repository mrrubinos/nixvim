{
  description = "Neovim configuration with nixvim";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixvim.url = "github:nix-community/nixvim";
    flake-utils.url = "github:numtide/flake-utils";
    gemini = {                                                          
      url = "github:meinside/gemini.nvim";
      flake = false;
    }; 
    template = {
      url = "github:nvimdev/template.nvim";
      flake = false;
    };
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        #nixvimLib = inputs.nixvim.lib.${system};
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        nixvim' = inputs.nixvim.legacyPackages.${system};
        mkPkgs = name: src: pkgs.vimUtils.buildVimPlugin { inherit name src; };
        nvim = nixvim'.makeNixvimWithModule {
          inherit pkgs;
          module = {
            # Global options
            config = {
              globals.mapleader = " ";
              opts = {
                number = true;
                relativenumber = false;
                tabstop = 2;
                shiftwidth = 2;
                expandtab = true;
                termguicolors = true;
                clipboard = "unnamedplus"; # Use system clipboard
              };
              colorschemes.ayu.enable = true;
              keymaps = [
                {
                  mode = "n";
                  key = "<leader>e";
                  action = ":Neotree toggle<CR>";
                  options = {
                    desc = "Toggle Explorer";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>,";
                  action = ":BufferLineCyclePrev<CR>";
                  options = {
                    desc = "Switch Buffer";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>-";
                  action = ":split<CR>";
                  options = {
                    desc = "Split horizontal";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>|";
                  action = ":vsplit<CR>";
                  options = {
                    desc = "Split vertical";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>/";
                  action = ":CommentToggle<CR>";
                  options = {
                    desc = "Toggle comment";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>:";
                  action = "<cmd>Telescope command_history<CR>";
                  options = {
                    desc = "Command History";
                    silent = true;
                  };
                }
                # Buffer operations
                {
                  mode = "n";
                  key = "<leader>bb";
                  action = "<cmd>Telescope buffers<CR>";
                  options = {
                    desc = "Switch Buffer";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>bd";
                  action = ":bdelete<CR>";
                  options = {
                    desc = "Delete Buffer";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>bn";
                  action = ":bnext<CR>";
                  options = {
                    desc = "Next Buffer";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>bp";
                  action = ":bprevious<CR>";
                  options = {
                    desc = "Previous Buffer";
                    silent = true;
                  };
                }
                # File operations
                {
                  mode = "n";
                  key = "<leader>ff";
                  action = "<cmd>Telescope find_files<CR>";
                  options = {
                    desc = "Find File";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>fg";
                  action = "<cmd>Telescope live_grep<CR>";
                  options = {
                    desc = "Find Text";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>fr";
                  action = "<cmd>Telescope oldfiles<CR>";
                  options = {
                    desc = "Recent Files";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>fn";
                  action = ":enew<CR>";
                  options = {
                    desc = "New File";
                    silent = true;
                  };
                }
                # Git operations
                {
                  mode = "n";
                  key = "<leader>gg";
                  action = ":LazyGit<CR>";
                  options = {
                    desc = "LazyGit";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>gb";
                  action = "<cmd>Telescope git_branches<CR>";
                  options = {
                    desc = "Branches";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>gc";
                  action = "<cmd>Telescope git_commits<CR>";
                  options = {
                    desc = "Commits";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>gs";
                  action = "<cmd>Telescope git_status<CR>";
                  options = {
                    desc = "Status";
                    silent = true;
                  };
                }
                # LSP operations
                {
                  mode = "n";
                  key = "<leader>la";
                  action = ":lua vim.lsp.buf.code_action()<CR>";
                  options = {
                    desc = "Code Action";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>ld";
                  action = ":lua vim.lsp.buf.definition()<CR>";
                  options = {
                    desc = "Definition";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>lD";
                  action = ":lua vim.lsp.buf.declaration()<CR>";
                  options = {
                    desc = "Declaration";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>li";
                  action = ":lua vim.lsp.buf.implementation()<CR>";
                  options = {
                    desc = "Implementation";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>lr";
                  action = ":lua vim.lsp.buf.references()<CR>";
                  options = {
                    desc = "References";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>lR";
                  action = ":lua vim.lsp.buf.rename()<CR>";
                  options = {
                    desc = "Rename";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>lh";
                  action = ":lua vim.lsp.buf.hover()<CR>";
                  options = {
                    desc = "Hover";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>lf";
                  action = ":lua vim.lsp.buf.format()<CR>";
                  options = {
                    desc = "Format";
                    silent = true;
                  };
                }
                # Search operations
                {
                  mode = "n";
                  key = "<leader>sh";
                  action = "<cmd>Telescope help_tags<CR>";
                  options = {
                    desc = "Help Tags";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>sk";
                  action = "<cmd>Telescope keymaps<CR>";
                  options = {
                    desc = "Keymaps";
                    silent = true;
                  };
                }
                {
                  mode = "n";
                  key = "<leader>sc";
                  action = "<cmd>Telescope commands<CR>";
                  options = {
                    desc = "Commands";
                    silent = true;
                  };
                }
              ];

              # Plugin configurations
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
                        desc = "Find File";
                        key = "f";
                        keymap = "<leader>ff";
                        action = "Telescope find_files";
                      }
                      {
                        icon = " ";
                        desc = "New File";
                        key = "n";
                        action = "enew";
                      }
                      {
                        icon = " ";
                        desc = "Find Text";
                        key = "t";
                        keymap = "<leader>fg";
                        action = "Telescope live_grep";
                      }
                      {
                        icon = "  ";
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
                    ensure_installed = ["bash" "erlang" "elixir" "markdown" "markdown_inline" "nix"];
                  };
                };
                web-devicons.enable = true;
                # Which-key configuration
                which-key = {
                  enable = true;
                };
              };
              extraPlugins = with pkgs.vimPlugins; [
                lazygit-nvim
                (pkgs.vimUtils.buildVimPlugin {
                  pname = "gemini-nvim";
                  version = "latest";
                  src = inputs.gemini;
                  dependencies = with pkgs.vimPlugins; [
                    plenary-nvim
                  ];
                })
                (mkPkgs "template" inputs.template)
              ];
              extraConfigLua = # lua
                ''
                  require('gemini').setup({
                     configFilepath = '~/.config/gemini.nvim/config.json',
                     timeout = 30 * 1000,
                     model = 'gemini-1.5-pro-latest',
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
      '';
            };
          };
        };
      in
        {
        packages.default = nvim;

        apps.default = {
          type = "app";
          program = "${nvim}/bin/nvim";
        };
      });
}
