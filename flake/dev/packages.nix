{ inputs, lib, ... }:
{
  perSystem =
    { pkgs, system, ... }:
    {
      packages =
        let
          universalPackages = {
            docs = import ../../docs { inherit pkgs inputs lib; };
          };

          # Testbeds are virtual machines based on NixOS, therefore they are
          # only available for Linux systems.
          testbedPackages = lib.optionalAttrs (lib.hasSuffix "-linux" system) (
            import ../../stylix/testbed.nix { inherit pkgs inputs lib; }
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
        universalPackages // testbedPackages';
    };
}
