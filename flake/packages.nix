{
  lib,
  inputs,
  self,
  ...
}:
{

  perSystem =
    { pkgs, system, ... }:
    {
      packages =
        let
          # Testbeds are virtual machines based on NixOS, therefore they are
          # only available for Linux systems.
          testbedPackages = lib.mkIf (lib.hasSuffix "-linux" system) (
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
            docs = import "${self}/docs" { inherit pkgs inputs lib; };
            palette-generator = pkgs.callPackage "${self}/palette-generator" { };
          }
        ];
    };
}
