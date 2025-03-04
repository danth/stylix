inputs:
{ lib, ... }:

# Imported modules which define new options must use an absolute path based
# on ${inputs.self}, otherwise those options will not appear in the generated
# documentation.

let
  autoload = import ../autoload.nix { inherit lib inputs; } "darwin";
in
{
  imports = [
    "${inputs.self}/stylix/darwin/fonts.nix"
    "${inputs.self}/stylix/darwin/palette.nix"
    "${inputs.self}/stylix/fonts.nix"
    "${inputs.self}/stylix/home-manager-integration.nix"
    "${inputs.self}/stylix/opacity.nix"
    "${inputs.self}/stylix/palette.nix"
    "${inputs.self}/stylix/pixel.nix"
    "${inputs.self}/stylix/target.nix"
  ] ++ autoload;
}
