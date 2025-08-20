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
        
        # Function to create nixvim with custom PKM path
        mkNixvim = pkmPath: nixvim'.makeNixvimWithModule {
          inherit pkgs;
          module = {
            imports = [
              ./options.nix
              ./theme.nix
              (import ./plugins.nix { inherit inputs pkgs pkmPath; })
            ];
            # Global options
            config = {
              globals.mapleader = " ";
              
              # Add para.lua to the Neovim runtime
              extraFiles = {
                "lua/para.lua".source = ./custom_plugins/para.lua;
              };
              
              keymaps = import ./keymaps.nix;
            };
          };
        };
        
        # Default nixvim with default PKM path
        nvim = mkNixvim "~/Documents/PARA";
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

        # Wrapper script that can accept PKM path
        packages.nvim-with-pkm = pkgs.writeShellScriptBin "nvim" ''
          # Allow setting PKM path via environment variable
          export NIXVIM_PKM_PATH="''${NIXVIM_PKM_PATH:-~/Documents/PARA}"
          exec ${nvim}/bin/nvim "$@"
        '';

        apps.default = {
          type = "app";
          program = "${nvim}/bin/nvim";
        };
        
        apps.para = {
          type = "app";
          program = "${para-cli}/bin/para";
        };
        
        # Export a lib function to create custom nixvim
        lib.mkNixvimWithPkm = pkmPath: mkNixvim pkmPath;
      });
}