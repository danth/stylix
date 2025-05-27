{
  lib,
  inputs,
  self,
  ...
}:
{

  perSystem =
    { pkgs, config, ... }:
    {
      # Build all packages with 'nix flake check' instead of only verifying they
      # are derivations.
      checks = config.packages;

      packages = lib.mkMerge [
        # Testbeds are virtual machines based on NixOS, therefore they are
        # only available for Linux systems.
        (lib.mkIf pkgs.stdenv.hostPlatform.isLinux (
          import "${self}/stylix/testbed/default.nix" {
            inherit pkgs inputs lib;
          }
        ))
        {
          docs = pkgs.callPackage "${self}/doc" {
            inherit inputs;
            inherit (inputs.nixpkgs.lib) nixosSystem;
            inherit (inputs.home-manager.lib) homeManagerConfiguration;
          };
          palette-generator = pkgs.callPackage "${self}/palette-generator" { };
        }
      ];
    };
}
