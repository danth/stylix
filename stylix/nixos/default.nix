inputs:
{
  lib,
  config,
  ...
}:

# Imported modules which define new options must use an absolute path based
# on ${inputs.self}, otherwise those options will not appear in the generated
# documentation.

let
  autoload = import ../autoload.nix { inherit lib inputs; } "nixos";
in
{
  imports = [
    "${inputs.self}/stylix/cursor.nix"
    "${inputs.self}/stylix/fonts.nix"
    "${inputs.self}/stylix/home-manager-integration.nix"
    "${inputs.self}/stylix/nixos/cursor.nix"
    "${inputs.self}/stylix/nixos/fonts.nix"
    "${inputs.self}/stylix/nixos/palette.nix"
    "${inputs.self}/stylix/opacity.nix"
    "${inputs.self}/stylix/palette.nix"
    "${inputs.self}/stylix/pixel.nix"
    "${inputs.self}/stylix/target.nix"
    "${inputs.self}/stylix/release.nix"
    (import "${inputs.self}/stylix/overlays.nix" inputs)
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
