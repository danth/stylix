{ lib, ... }:

with lib;

{
  imports = [
    ./base16.nix
    ./colors.nix
    ./fonts.nix
    ./home-manager.nix
    ./pixel.nix
  ];

  options.stylix.image = mkOption {
    type = types.package;
    description = "Wallpaper image.";
  };
}
