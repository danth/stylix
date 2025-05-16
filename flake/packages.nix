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
      # We want docs, palette-generator and testbeds to be checked when running
      # `nix flake check` or `stylix-check`
      checks = config.packages;

      packages =
        let
          # Testbeds are virtual machines based on NixOS, therefore they are
          # only available for Linux systems.
          testbedPackages = lib.optionalAttrs (lib.hasSuffix "-linux" system) (
            import "${self}/stylix/testbed.nix" { inherit pkgs inputs lib; }
          );

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
          testbedPackages'
          {
            docs = pkgs.callPackage "${self}/docs" {
              inherit inputs;
              inherit (inputs.nixpkgs.lib) nixosSystem;
              inherit (inputs.home-manager.lib) homeManagerConfiguration;
            };
            palette-generator = pkgs.callPackage "${self}/palette-generator" { };
          }
        ];
    };
}
