{
  lib,
  config,
  ...
}:

let
  autoload = import ../autoload.nix { inherit lib; } "nixos";
in
{
  imports = [
    ./cursor.nix
    ./palette.nix
    ../cursor.nix
    ../fonts.nix
    ../home-manager-integration.nix
    ../opacity.nix
    ../palette.nix
    ../pixel.nix
    ../target.nix
    ../release.nix
    ../overlays.nix
  ] ++ autoload;
  config.warnings =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.enableReleaseChecks
        && (config.stylix.release != config.system.nixos.release)
      )
      [
        ''
          You are using different Stylix and NixOS versions. This is
          likely to cause errors and unexpected behavior. It is highly
          recommended that you use a version of Stylix that matches your chosen
          version of NixOS.

          If you are willing to accept the risks that come with using
          mismatched versions, you may disable this warning by adding

              stylix.enableReleaseChecks = false;

          to your configuration.
        ''
      ];
}
