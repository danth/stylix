{
  config,
  lib,
  ...
}: let
  settings = {
    preload = ["${config.stylix.image}"];
    wallpaper = [" ,${config.stylix.image}"];
  };
in {
  options.stylix.targets.hyprpaper.enable =
    config.lib.stylix.mkEnableTarget "Hyprpaper" true;

  config =
    lib.mkIf config.stylix.targets.hyprland.enable
    {
      services.hyprpaper = {
        inherit settings;
      };
    };
}
