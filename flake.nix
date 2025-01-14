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

    git-hooks = {
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs-stable.follows = "git-hooks/nixpkgs";
        nixpkgs.follows = "nixpkgs";
      };

      url = "github:cachix/git-hooks.nix";
    };

    gnome-shell = {
      flake = false;

      # TODO: Unlocking the input and pointing to official repository requires
      # updating the patch:
      # https://github.com/danth/stylix/pull/224#discussion_r1460339607.
      url = "github:GNOME/gnome-shell/47.2";
    };

    # The 'home-manager' input is used to generate the documentation.
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Interface flake systems.
    systems.url = "github:nix-systems/default";

    tinted-foot = {
      flake = false;

      # Lock the tinted-foot input to prevent upstream breaking changes.
      #
      # Considering that Stylix eventually re-implements this input's
      # functionality [1], it might be easiest to lock this input to avoid
      # wasted maintenance effort.
      #
      # [1]: https://github.com/danth/stylix/issues/571
      url = "github:tinted-theming/tinted-foot/fd1b924b6c45c3e4465e8a849e67ea82933fcbe4";
    };

    tinted-zed = {
      flake = false;
      url = "github:tinted-theming/base16-zed";
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

    firefox-gnome-theme = {
      flake = false;
      url = "github:rafaelmardojai/firefox-gnome-theme";
    };
  };

  outputs =
    {
      nixpkgs,
      base16,
      self,
      ...
    }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        inherit (nixpkgs) lib;
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        checks = lib.attrsets.unionOfDisjoint {
          git-hooks = inputs.git-hooks.lib.${system}.run {
            hooks = {
              deadnix.enable = true;
              hlint.enable = true;

              nixfmt-rfc-style = {
                enable = true;
                settings.width = 80;
              };

              statix.enable = true;
              stylish-haskell.enable = true;
              typos.enable = true;
              yamllint.enable = true;
            };

            src = ./.;
          };
        } self.packages.${system};

        devShells = {
          default = pkgs.mkShell {
            inherit (self.checks.${system}.git-hooks) shellHook;

            packages = [
              inputs.home-manager.packages.${system}.default
              self.checks.${system}.git-hooks.enabledPackages
            ];
          };

          ghc = pkgs.mkShell {
            inputsFrom = [ self.devShells.${system}.default ];
            packages = [ pkgs.ghc ];
          };
        };

        packages =
          let
            universalPackages = {
              docs = import ./docs { inherit pkgs inputs lib; };

              nix-flake-check = pkgs.writeShellApplication {
                meta.description = "A parallelized alternative to 'nix flake check'";
                name = "nix-flake-check";

                runtimeInputs = with pkgs; [
                  nix
                  parallel
                ];

                text = ''
                  nix flake show --json --no-update-lock-file |
                    jq --raw-output '
                      ((.checks."${system}" // {}) | keys) as $checks |
                      ((.packages."${system}" // {}) | keys) as $packages |
                      (($checks - $packages)[] | "checks.${system}.\(.)"),
                      ($packages[] | "packages.${system}.\(.)")
                    ' |
                    parallel --halt now,fail=1 '
                      nix build --no-update-lock-file --verbose .#{}
                    '
                '';
              };

              palette-generator = pkgs.callPackage ./palette-generator { };
            };

            # Testbeds are virtual machines based on NixOS, therefore they are
            # only available for Linux systems.
            testbedPackages = lib.optionalAttrs (lib.hasSuffix "-linux" system) (
              import ./stylix/testbed.nix { inherit pkgs inputs lib; }
            );
          in
          universalPackages // testbedPackages;
      }
    )
    // {
      nixosModules.stylix =
        { pkgs, ... }@args:
        {
          imports = [
            (import ./stylix/nixos inputs {
              inherit (self.packages.${pkgs.system}) palette-generator;
              base16 = base16.lib args;
              homeManagerModule = self.homeManagerModules.stylix;
            })
          ];
        };

      homeManagerModules.stylix =
        { pkgs, ... }@args:
        {
          imports = [
            (import ./stylix/hm inputs {
              inherit (self.packages.${pkgs.system}) palette-generator;
              base16 = base16.lib args;
            })
          ];
        };

      darwinModules.stylix =
        { pkgs, ... }@args:
        {
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
