{
  lib,
  inputs,
  self,
  config,
  ...
}:
let
  packageAliasNames = lib.pipe config.stylix.perSystemAliases [
    (builtins.filter (a: a.output == "packages"))
    (builtins.catAttrs "old")
  ];
in
{

  perSystem =
    { pkgs, config, ... }:
    {
      # Build all packages with 'nix flake check' instead of only verifying they
      # are derivations.
      checks = builtins.removeAttrs config.packages packageAliasNames;

      packages = lib.mkMerge [
        # Testbeds are virtual machines based on NixOS, therefore they are
        # only available for Linux systems.
        (lib.mkIf pkgs.stdenv.hostPlatform.isLinux (
          import "${self}/stylix/testbed.nix" {
            inherit pkgs inputs lib;
          }
        ))
        {
          doc = pkgs.callPackage "${self}/doc" {
            inherit inputs;
            inherit (inputs.nixpkgs.lib) nixosSystem;
            inherit (inputs.home-manager.lib) homeManagerConfiguration;
          };
          palette-generator = pkgs.callPackage "${self}/palette-generator" { };
        }
      ];
    };
}
