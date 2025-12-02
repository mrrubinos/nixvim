{
  description = "Neovim configuration with nixvim";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixvim.url = "github:nix-community/nixvim";
    flake-utils.url = "github:numtide/flake-utils";
    gmn = {                                                          
      url = "github:meinside/gmn.nvim";
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
        
        nvim = nixvim'.makeNixvimWithModule {
          inherit pkgs;
          module = {
            imports = [
              ./options.nix
              ./theme.nix
              (import ./plugins.nix { inherit inputs pkgs; })
            ];
            # Global options
            config = {
              globals.mapleader = " ";
              
              # Add para.lua to the Neovim runtime
              extraFiles = {
                "lua/para.lua".source = ./custom_plugins/para.lua;
              };
              
              autoCmd = [
                {
                  event = "VimEnter";
                  callback = {
                    __raw = ''
                      function(data)
                        local directory = vim.fn.isdirectory(data.file) == 1
                        if not directory then
                          return
                        end
                        vim.cmd.cd(data.file)
                        require("neo-tree.command").execute({ action = "show" })
                      end
                    '';
                  };
                }
              ];
              
              keymaps = import ./keymaps.nix;
            };
          };
        };
      in
        let
          para-cli = pkgs.writeShellScriptBin "para" ''
            export PARA_BASE="''${PARA_BASE:-~/Documents/PARA}"
            export PARA_LOGS="$PARA_BASE/logs"
            exec ${pkgs.bash}/bin/bash ${./para} "$@"
          '';
        in
        {
        packages.default = nvim;
        
        # PARA CLI script
        packages.para-cli = para-cli;

        apps.default = {
          type = "app";
          program = "${nvim}/bin/nvim";
        };
        
        apps.para = {
          type = "app";
          program = "${para-cli}/bin/para";
        };
      });
}