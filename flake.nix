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
        # Make PKM path configurable via environment variable or default
        pkmPath = builtins.getEnv "NIXVIM_PKM_PATH";
        defaultPkmPath = "~/Documents/PARA";
        finalPkmPath = if pkmPath != "" then pkmPath else defaultPkmPath;
        
        nvim = nixvim'.makeNixvimWithModule {
          inherit pkgs;
          module = {
            imports = [
              ./options.nix
              ./theme.nix
              (import ./plugins.nix { inherit inputs pkgs; pkmPath = finalPkmPath; })
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
      in
        {
        packages.default = nvim;
        
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
      });
}