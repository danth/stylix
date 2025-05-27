{ pkgs, ... }:
let
  images = pkgs.callPackages ../images.nix { };
in
{
  stylix = {
    enable = true;
    image = images.dark;
    polarity = "dark";
    cursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      size = 32;
    };
  };
}
