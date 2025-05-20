{
  lib,
  inputs,
  self,
  ...
}:
{

  perSystem =
    {
      pkgs,
      system,
      config,
      ...
    }:
    {
      # Build all packages with 'nix flake check' instead of only verifying they
      # are derivations.
      checks = config.packages;

      packages =
        let
          testbedPackages = import "${self}/stylix/testbed.nix" {
            inherit pkgs inputs lib;
          };

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
        lib.mkMerge [
          # Testbeds are virtual machines based on NixOS, therefore they are
          # only available for Linux systems.
          (lib.mkIf pkgs.stdenv.hostPlatform.isLinux testbedPackages')
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
