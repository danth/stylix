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

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
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
      packages = genAttrs [ "aarch64-darwin" "aarch64-linux" "i686-linux" "x86_64-darwin" "x86_64-linux" ] (
        system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          palette-generator = pkgs.callPackage ./palette-generator { };
        }
      );

      nixosModules.stylix = { pkgs, ... }@args: {
        imports = [
          (import ./stylix/nixos {
            inherit (self.packages.${pkgs.system}) palette-generator;
            base16 = base16.lib args;
            homeManagerModule = self.homeManagerModules.stylix;
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

      darwinModules.stylix = { pkgs, ... }@args: {
        imports = [
          (import ./stylix/darwin {
            inherit (self.package.${pkgs.system}) palette-generator;
            base16 = base16.lib args;
            homeManagerModule = self.homeManagerModules.stylix;
          })
        ];
      };
    };
}
