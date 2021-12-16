{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, self, ... }:
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
            buildPhase = "ghc -O -threaded -Wall -Wno-type-defaults Stylix/Main.hs";
            installPhase = "install -D Stylix/Main $out/bin/palette-generator";
          };

          palette-generator-app = utils.lib.mkApp {
            drv = palette-generator;
            name = "palette-generator";
          };

        in {
          packages.palette-generator = palette-generator;
          apps.palette-generator = palette-generator-app;
        })) // {
          nixosModules.stylix = { pkgs, ... }: {
            imports = [
              ./modules/console.nix
              ./modules/dunst.nix
              ./modules/feh.nix
              ./modules/fish.nix
              ./modules/grub.nix
              ./modules/gtk.nix
              ./modules/kitty.nix
              ./modules/lightdm.nix
              ./modules/plymouth
              ./modules/qutebrowser.nix
              ./modules/sway.nix
              ./modules/vim.nix
              (import ./stylix/palette.nix
                self.packages.${pkgs.system}.palette-generator)
              ./stylix/base16.nix
              ./stylix/fonts.nix
              ./stylix/home-manager.nix
              ./stylix/pixel.nix
            ];
          };
        };
}
