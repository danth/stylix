{
  lib,
  config,
  ...
}:

let
  autoload = import ../autoload.nix { inherit lib; } "darwin";
in
{
  imports = [
    ./palette.nix
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
        && (config.stylix.release != config.system.darwinRelease)
      )
      [
        ''
          You are using different Stylix and nix-darwin versions. This is
          likely to cause errors and unexpected behavior. It is highly
          recommended that you use a version of Stylix that matches your chosen
          version of nix-darwin.

          If you are willing to accept the risks that come with using
          mismatched versions, you may disable this warning by adding

              stylix.enableReleaseChecks = false;

          to your configuration.
        ''
      ];
}
