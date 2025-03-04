inputs:
{ lib, ... }:

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
  ] ++ autoload;
}
