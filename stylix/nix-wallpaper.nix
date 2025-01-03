{ pkgs ? import <nixpkgs> { } }:
rec {
  # Takes a scheme, resulting wallpaper height and width, plus logo scale, and ouputs the generated wallpaper path
  # Example:
  # wallpaper = nixWallpaperFromScheme {
  #   width = 2560;
  #   height = 1440;
  #   logoScale = 5.0;
  # };
  config.lib.stylix.flake = import ./flake-wallpaper.nix { inherit pkgs; };
  
  config.lib.stylix.pixel = import ./pixel-wallpaper.nix { inherit pkgs; };

}