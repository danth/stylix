{
  description = "Theming framework for NixOS, Home Manager, nix-darwin, and Nix-on-Droid";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    systems.url = "github:nix-systems/default";

    # keep-sorted start block=yes newline_separated=yes
    base16-fish = {
      url = "github:tomyun/base16-fish";
      flake = false;
    };

    base16-helix = {
      url = "github:tinted-theming/base16-helix";
      flake = false;
    };

    base16-vim = {
      # TODO: Unlock this input once [1] ("Seemingly bad parsing of whitespace
      # in abbriviated lists (affecting stylix's handling of base16-vim)") is
      # resolved, preventing us from fetching commit [2] ("fix(theme): Remove
      # illegal style attributes").
      #
      # [1]: https://github.com/SenchoPens/fromYaml/issues/1
      # [2]: https://github.com/tinted-theming/tinted-vim/commit/0508601eff146db2537eff23e93dd0c543914896
      url = "github:tinted-theming/base16-vim/577fe8125d74ff456cf942c733a85d769afe58b7";
      flake = false;
    };

    base16.url = "github:SenchoPens/base16.nix";

    firefox-gnome-theme = {
      url = "github:rafaelmardojai/firefox-gnome-theme";
      flake = false;
    };

    flake-compat.url = "github:edolstra/flake-compat";

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
      };
    };

    gnome-shell = {
      # TODO: Unlocking the input and pointing to official repository requires
      # updating the patch:
      # https://github.com/danth/stylix/pull/224#discussion_r1460339607.
      url = "github:GNOME/gnome-shell/47.2";
      flake = false;
    };

    # The 'home-manager' input is used to generate the documentation.
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tinted-foot = {
      # Lock the tinted-foot input to prevent upstream breaking changes.
      #
      # Considering that Stylix eventually re-implements this input's
      # functionality [1], it might be easiest to lock this input to avoid
      # wasted maintenance effort.
      #
      # [1]: https://github.com/danth/stylix/issues/571
      url = "github:tinted-theming/tinted-foot/fd1b924b6c45c3e4465e8a849e67ea82933fcbe4";
      flake = false;
    };

    tinted-kitty = {
      # Lock the tinted-kitty input to prevent upstream breaking changes.
      #
      # Considering that Stylix eventually re-implements this input's
      # functionality [1], it might be easiest to lock this input to avoid
      # wasted maintenance effort.
      #
      # [1]: https://github.com/danth/stylix/issues/534
      url = "github:tinted-theming/tinted-kitty/eb39e141db14baef052893285df9f266df041ff8";
      flake = false;
    };

    tinted-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    tinted-tmux = {
      url = "github:tinted-theming/tinted-tmux";
      flake = false;
    };

    tinted-zed = {
      url = "github:tinted-theming/base16-zed";
      flake = false;
    };
    # keep-sorted end
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
              editorconfig-checker.enable = true;
              hlint.enable = true;

              treefmt = {
                enable = true;
                package = self.formatter.${system};
              };

              statix.enable = true;
              typos = {
                enable = true;
                settings.configuration = ''
                  [default.extend-identifiers]
                  MrSom3body="MrSom3body"
                '';
              };
              yamllint.enable = true;
            };

            src = ./.;
          };
        } self.packages.${system};

        devShells = {
          default =
            let
              check = pkgs.writeShellApplication {
                name = "stylix-check";
                runtimeInputs = with pkgs; [
                  nix
                  nix-fast-build
                ];
                text = ''
                  cores="$(nproc)"
                  system="$(nix eval --expr builtins.currentSystem --impure --raw)"
                  nix-fast-build \
                    --eval-max-memory-size 512 \
                    --eval-workers "$cores" \
                    --flake ".#checks.$system" \
                    --no-link \
                    --skip-cached \
                    "$@"
                '';
              };
            in
            pkgs.mkShell {
              inherit (self.checks.${system}.git-hooks) shellHook;

              packages = [
                check
                inputs.home-manager.packages.${system}.default
                self.checks.${system}.git-hooks.enabledPackages
                self.formatter.${system}
              ] ++ self.formatter.${system}.runtimeInputs;
            };

          ghc = pkgs.mkShell {
            inputsFrom = [ self.devShells.${system}.default ];
            packages = [ pkgs.ghc ];
          };
        };

        formatter = pkgs.treefmt.withConfig {
          runtimeInputs = with pkgs; [
            nixfmt-rfc-style
            stylish-haskell
            keep-sorted
          ];

          settings = {
            on-unmatched = "info";
            tree-root-file = "flake.nix";

            formatter = {
              stylish-haskell = {
                command = "stylish-haskell";
                includes = [ "*.hx" ];
              };
              nixfmt = {
                command = "nixfmt";
                options = [ "--width=80" ];
                includes = [ "*.nix" ];
              };
              keep-sorted = {
                command = "keep-sorted";
                includes = [ "*" ];
              };
            };
          };
        };

        packages =
          let
            universalPackages = {
              docs = import ./docs { inherit pkgs inputs lib; };
              palette-generator = pkgs.callPackage ./palette-generator { };
            };

            # Testbeds are virtual machines based on NixOS, therefore they are
            # only available for Linux systems.
            testbedPackages = lib.optionalAttrs (lib.hasSuffix "-linux" system) (
              import ./stylix/testbed.nix { inherit pkgs inputs lib; }
            );

            # Discord is not available on arm64. This workaround filters out
            # testbeds using that package, until we have a better way to handle
            # this.
            testbedPackages' =
              if system == "aarch64-linux" then
                lib.filterAttrs (
                  name: _: !lib.hasPrefix "testbed:discord:vencord" name
                ) testbedPackages
              else
                testbedPackages;
          in
          universalPackages // testbedPackages';
      }
    )
    // {
      nixosModules.stylix =
        { pkgs, ... }@args:
        {
          imports = [
            (import ./stylix/nixos inputs)
            {
              stylix = {
                inherit inputs;
                paletteGenerator =
                  self.packages.${pkgs.stdenv.hostPlatform.system}.palette-generator;
                base16 = base16.lib args;
                homeManagerIntegration.module = self.homeManagerModules.stylix;
              };
            }
          ];
        };

      homeManagerModules.stylix =
        { pkgs, ... }@args:
        {
          imports = [
            (import ./stylix/hm inputs)
            {
              stylix = {
                inherit inputs;
                paletteGenerator =
                  self.packages.${pkgs.stdenv.hostPlatform.system}.palette-generator;
                base16 = base16.lib args;
              };
            }
          ];
        };

      darwinModules.stylix =
        { pkgs, ... }@args:
        {
          imports = [
            (import ./stylix/darwin inputs)
            {
              stylix = {
                inherit inputs;
                paletteGenerator =
                  self.packages.${pkgs.stdenv.hostPlatform.system}.palette-generator;
                base16 = base16.lib args;
                homeManagerIntegration.module = self.homeManagerModules.stylix;
              };
            }
          ];
        };

      nixOnDroidModules.stylix =
        { pkgs, ... }@args:
        {
          imports = [
            (import ./stylix/droid inputs)
            {
              stylix = {
                paletteGenerator =
                  self.packages.${pkgs.stdenv.hostPlatform.system}.palette-generator;
                base16 = base16.lib args;
                homeManagerIntegration.module = self.homeManagerModules.stylix;
              };
            }
          ];
        };
    };
}
