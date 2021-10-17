{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, self, ... }:
    (utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        ghc = pkgs.haskellPackages.ghcWithPackages
          (haskellPackages: with haskellPackages; [ json JuicyPixels ]);

        palette-generator = pkgs.stdenvNoCC.mkDerivation {
          name = "palette-generator";
          src = ./palette-generator;
          buildInputs = [ ghc ];
          buildPhase = "ghc -O -threaded -Wall Main.hs";
          installPhase = "install -D Main $out/bin/palette-generator";
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
