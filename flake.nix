{
  inputs = {
    base16-fish = {
      flake = false;
      url = "github:tomyun/base16-fish";
    };

    base16-helix = {
      flake = false;
      url = "github:tinted-theming/base16-helix";
    };

    base16-vim = {
      flake = false;
      url = "github:tinted-theming/base16-vim";
    };

    base16.url = "github:SenchoPens/base16.nix";

    flake-compat = {
      flake = false;
      url = "github:edolstra/flake-compat";
    };

    flake-utils = {
      inputs.systems.follows = "systems";
      url = "github:numtide/flake-utils";
    };

    gnome-shell = {
      flake = false;

      # TODO: Unlocking the input and pointing to official repository requires
      # updating the patch:
      # https://github.com/danth/stylix/pull/224#discussion_r1460339607.
      url = "github:GNOME/gnome-shell/46.1";
    };

    # The 'home-manager' input is used to generate the documentation.
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Interface flake systems.
    systems.url = "github:nix-systems/default";

    tinted-foot = {
      flake = false;
      url = "github:tinted-theming/tinted-foot";
    };

    tinted-tmux = {
      flake = false;
      url = "github:tinted-theming/tinted-tmux";
    };

    tinted-kitty = {
      flake = false;

      # Lock the tinted-kitty input to prevent upstream breaking changes.
      #
      # Considering that Stylix eventually re-implements this input's
      # functionality [1], it might be easiest to lock this input to avoid
      # wasted maintenance effort.
      #
      # [1]: https://github.com/danth/stylix/issues/534
      url = "github:tinted-theming/tinted-kitty/eb39e141db14baef052893285df9f266df041ff8";
    };
  };

  outputs =
    { nixpkgs, base16, self, ... }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system: let
        inherit (nixpkgs) lib;
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = let
            universalPackages = {
              docs = import ./docs { inherit pkgs inputs lib; };
              palette-generator = pkgs.callPackage ./palette-generator { };
            };

            # Testbeds are virtual machines based on NixOS, therefore they are
            # only available for Linux systems.
            testbedPackages = lib.optionalAttrs
              (lib.hasSuffix "-linux" system)
              (import ./stylix/testbed.nix { inherit pkgs inputs lib; });
          in
            universalPackages // testbedPackages;
      }
    )
    // {
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
