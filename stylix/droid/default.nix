inputs:
{ lib, ... }:

let
  autoload = import ../autoload.nix { inherit lib inputs; } "droid";
in
{
  imports = [
    "${inputs.self}/stylix/fonts.nix"
    "${inputs.self}/stylix/home-manager-integration.nix"
    "${inputs.self}/stylix/opacity.nix"
    "${inputs.self}/stylix/palette.nix"
    "${inputs.self}/stylix/pixel.nix"
    "${inputs.self}/stylix/target.nix"
  ] ++ autoload;

  # See https://github.com/nix-community/nix-on-droid/issues/436
  options.lib = lib.mkOption {
    type = with lib.types; attrsOf attrs;
  };
}
