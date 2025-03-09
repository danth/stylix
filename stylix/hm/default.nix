inputs:
{ lib, ... }:

# Imported modules which define new options must use an absolute path based
# on ${inputs.self}, otherwise those options will not appear in the generated
# documentation.

let
  autoload = import ../autoload.nix { inherit lib inputs; } "hm";
in
{
  imports = [
    "${inputs.self}/stylix/cursor.nix"
    "${inputs.self}/stylix/fonts.nix"
    "${inputs.self}/stylix/hm/cursor.nix"
    "${inputs.self}/stylix/hm/fonts.nix"
    "${inputs.self}/stylix/hm/icon.nix"
    "${inputs.self}/stylix/hm/palette.nix"
    "${inputs.self}/stylix/icon.nix"
    "${inputs.self}/stylix/opacity.nix"
    "${inputs.self}/stylix/palette.nix"
    "${inputs.self}/stylix/pixel.nix"
    "${inputs.self}/stylix/target.nix"
  ] ++ autoload;
}
