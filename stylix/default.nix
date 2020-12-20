{ lib, ... }:

with lib;

{
  imports = [
    ./colors.nix
    ./fonts.nix
    ./home-manager.nix
  ];

  options.stylix.image = mkOption {
    type = types.package;
    description = "Wallpaper image.";
  };
}
