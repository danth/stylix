args:
{ config, lib, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config.xdg.configFile = {
    "stylix/wallpaper".source = if (config.lib.stylix.types.static.check config.stylix.wallpaper) then config.stylix.wallpaper.image
    else if (config.lib.stylix.types.animation.check config.stylix.wallpaper) then config.stylix.wallpaper.animation 
    else if (config.lib.stylix.types.video.check config.stylix.wallpaper) then config.stylix.wallpaper.video 
    else config.stylix.wallpaper.images;
    "stylix/palette.json".source = config.stylix.colors {
      template = ../palette.json.mustache;
      extension = ".json";
    };
    "stylix/palette.html".source = config.stylix.colors {
      template = ../palette.html.mustache;
      extension = ".html";
    };
  };
}
