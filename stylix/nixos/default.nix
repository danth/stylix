inputs:
{ palette-generator, base16, homeManagerModule }:
{ lib, ... }:

let
  autoload = import ../autoload.nix { inherit lib; } "nixos";
in {
  imports = [
    ../nix-wallpaper.nix
    ../target.nix
    ../opacity.nix
    ./cursor.nix
    ./fonts.nix
    (import ./palette.nix { inherit palette-generator base16; })
    (import ../templates.nix inputs)
    (import ../home-manager-integration.nix homeManagerModule)
  ] ++ autoload;
}
