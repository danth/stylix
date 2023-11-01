inputs:
{ palette-generator, base16 }:
{ config, lib, ... }:

let
  autoload = import ../autoload.nix { inherit lib; } "hm";
in {
  imports = [
    ../target.nix
    ../opacity.nix
    ./cursor.nix
    ./fonts.nix
    (import ./xdg.nix { inherit palette-generator base16; })
    (import ../templates.nix inputs)
  ] ++ autoload;
}
