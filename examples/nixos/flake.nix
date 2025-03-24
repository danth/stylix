{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, stylix, ... }:
    {
      nixosConfigurations.default = nixpkgs.lib.nixosSystem {
        modules = [
          ./configuration.nix
          stylix.nixosModules.stylix
        ];
      };
    };
}
