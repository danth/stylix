{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Pinned due to https://github.com/SenchoPens/base16.nix/issues/7
    base16.url = "github:SenchoPens/base16.nix/f493d8a8a6b4c1d814790e2189f26d4bcf433185";

    # Used for documentation
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
            inherit (self.packages.${pkgs.system}) palette-generator;
            base16 = base16.lib args;
            homeManagerModule = self.homeManagerModules.stylix;
          })
        ];
      };
    };
}
