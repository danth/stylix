{ lib, pkgs, ... }:

let
  package = pkgs.nsxiv;
in
{
  stylix.testbed.ui.command.text =
    let
      image = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/NixOS/nixos-artwork/4ad062cee62116f6055e2876e9638e7bb399d219/logo/nixos.svg.png";
        hash = "sha256-9+OfqfP5LmubdTcwBkS/AnOX4wZI2tKHLu5nhi43xcc=";
      };
    in
    ''
      # Xresources isn't loaded by default, so we force it
      ${lib.getExe pkgs.xorg.xrdb} ~/.Xresources
      ${lib.getExe package} ${image}
    '';

  home-manager.sharedModules = lib.singleton {
    home.packages = [ package ];
  };
}
