{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    base16 = {
      url = "github:SenchoPens/base16.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Used for documentation
    coricamu = {
      url = "github:danth/coricamu";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, base16, coricamu, self, ... }@inputs:
    with nixpkgs.lib;

    let
      docsOutputs = coricamu.lib.generateFlakeOutputs {
        outputName = "docs";
        modules = [ ./docs/default.nix ];
        specialArgs = { inherit inputs; };
      };

    in recursiveUpdate docsOutputs {
      packages = genAttrs [ "aarch64-linux" "i686-linux" "x86_64-linux" ] (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          ghc =
            pkgs.haskellPackages.ghcWithPackages
            (ps: with ps; [ json JuicyPixels random ]);

        in {
          palette-generator = pkgs.stdenvNoCC.mkDerivation {
            name = "palette-generator";
            src = ./palette-generator;
            buildInputs = [ ghc ];
            buildPhase = ''
              ghc -O -threaded -Wall -Wno-type-defaults Stylix/Main.hs
            '';
            installPhase = ''
              install -D Stylix/Main $out/bin/palette-generator
            '';
          };
        }
      );

      hydraJobs = {
        inherit (self.packages.x86_64-linux) docs palette-generator;
      };

      nixosModules.stylix = { pkgs, ... }@args: {
        imports = [
          (import ./stylix/nixos {
            inherit (self.packages.${pkgs.system}) palette-generator;
            base16 = base16.lib args;
          })
        ];
      };

      homeManagerModules.stylix = { pkgs, ... }@args: {
        imports = [
          (import ./stylix/hm {
            inherit (self.packages.${pkgs.system}) palette-generator;
            base16 = base16.lib args;
          })
        ];
      };
    };
}
