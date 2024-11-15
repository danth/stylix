inputs:
{ palette-generator, base16 }:
{ lib, ... }:

let
  autoload = import ../autoload.nix { inherit lib inputs; } "hm";
in
{
  imports = [
    ../pixel.nix
    ../target.nix
    ../opacity.nix
    ./cursor.nix
    ./fonts.nix
    ./icon.nix
    (import ./palette.nix { inherit palette-generator base16; })
    (import ../templates.nix inputs)
  ] ++ autoload;
}
