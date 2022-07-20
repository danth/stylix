{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    base16 = {
      url = "github:SenchoPens/base16.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, base16, utils, self, ... }:
    (utils.lib.eachSystem [ "aarch64-linux" "i686-linux" "x86_64-linux" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          ghc = pkgs.haskellPackages.ghcWithPackages (haskellPackages:
            with haskellPackages; [
              json
              JuicyPixels
              random
            ]);

          palette-generator = pkgs.stdenvNoCC.mkDerivation {
            name = "palette-generator";
            src = ./palette-generator;
            buildInputs = [ ghc ];
            buildPhase =
              "ghc -O -threaded -Wall -Wno-type-defaults Stylix/Main.hs";
            installPhase = "install -D Stylix/Main $out/bin/palette-generator";
          };

          # Internal documentation
          palette-generator-haddock = pkgs.stdenvNoCC.mkDerivation {
            name = "palette-generator-haddock";
            src = ./palette-generator;
            buildInputs = [ ghc ];
            buildPhase =
              "haddock $src/**/*.hs --html --ignore-all-exports --odir $out";
            dontInstall = true;
            dontFixup = true;
          };

          palette-generator-app = utils.lib.mkApp {
            drv = palette-generator;
            name = "palette-generator";
          };

        in {
          packages = {
            inherit palette-generator palette-generator-haddock;
          };
          apps.palette-generator = palette-generator-app;
        })) // {
          nixosModules.stylix = { pkgs, ... }@args: {
            imports = [
              ./modules/alacritty.nix
              ./modules/console.nix
              ./modules/dunst.nix
              ./modules/feh.nix
              ./modules/fish.nix
              ./modules/grub.nix
              ./modules/gtk.nix
              ./modules/helix.nix
              ./modules/kitty.nix
              ./modules/lightdm.nix
              ./modules/plymouth
              ./modules/qutebrowser.nix
              ./modules/sway.nix
              ./modules/vim.nix
              (import ./stylix/palette.nix {
                inherit (self.packages.${pkgs.system}) palette-generator;
                base16 = base16.lib args;
              })
              ./stylix/fonts.nix
              ./stylix/pixel.nix
            ];
          };
        };
}
