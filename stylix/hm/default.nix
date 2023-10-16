inputs:
{ palette-generator, base16 }:
{ config, lib, ... }:

let
  autoload = import ../autoload.nix { inherit lib; } "hm";
in {
  imports = [
    ../pixel.nix
    ../target.nix
    ../opacity.nix
    ./cursor.nix
    ./fonts.nix
    (import ./palette.nix { inherit palette-generator base16; })
    (import ../templates.nix inputs)
  ] ++ autoload;
}
