{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    base16.url = "github:SenchoPens/base16.nix";

    # Used for documentation
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # Templates
    base16-alacritty = {
      url = "github:aarowill/base16-alacritty";
      flake = false;
    };
    base16-alacritty-yaml = {
      url = "github:aarowill/base16-alacritty/63d8ae5dfefe5db825dd4c699d0cdc2fc2c3eaf7";
      flake = false;
    };
    base16-fish = {
      url = "github:tomyun/base16-fish";
      flake = false;
    };
    base16-foot = {
      url = "github:tinted-theming/base16-foot";
      flake = false;
    };
    base16-helix = {
      url = "github:tinted-theming/base16-helix";
      flake = false;
    };
    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };
    base16-tmux = {
      url = "github:tinted-theming/base16-tmux";
      flake = false;
    };
    base16-vim = {
      url = "github:chriskempson/base16-vim";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, base16, self, ... }@inputs:
    {
      packages = nixpkgs.lib.genAttrs [
        "aarch64-darwin"
        "aarch64-linux"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ] (
        system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          docs = import ./docs {
            inherit pkgs inputs;
            inherit (nixpkgs) lib;
          };

          palette-generator = pkgs.callPackage ./palette-generator { };
        }
      );

      nixosModules.stylix = { pkgs, ... }@args: {
        imports = [
          (import ./stylix/nixos inputs {
            inherit (self.packages.${pkgs.system}) palette-generator;
            base16 = base16.lib args;
            homeManagerModule = self.homeManagerModules.stylix;
          })
        ];
      };

      homeManagerModules.stylix = { pkgs, ... }@args: {
        imports = [
          (import ./stylix/hm inputs {
            inherit (self.packages.${pkgs.system}) palette-generator;
            base16 = base16.lib args;
          })
        ];
      };

      darwinModules.stylix = { pkgs, ... }@args: {
        imports = [
          (import ./stylix/darwin inputs {
            inherit (self.packages.${pkgs.system}) palette-generator;
            base16 = base16.lib args;
            homeManagerModule = self.homeManagerModules.stylix;
          })
        ];
      };
    };
}
