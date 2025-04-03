inputs:
{
  lib,
  pkgs,
  config,
  ...
}:
{
  options.stylix.overlays.enable = config.lib.stylix.mkEnableTarget "packages via overlays" true;

  imports = map (
    f:
    let
      file = import f;
      attrs =
        if builtins.typeOf file == "lambda" then
          file { inherit lib pkgs config; }
        else
          file;
    in
    {
      options = attrs.options or { };
      config.nixpkgs.overlays = lib.mkIf config.stylix.overlays.enable [
        attrs.overlay
      ];
    }
  ) (import ./autoload.nix { inherit lib inputs; } "overlay");
}
