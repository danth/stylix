args:
{ config, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config.environment.etc = {
    "stylix/wallpaper".source = config.stylix.wallpaper.unpack {
      image = { file, ... }: file;
      animation = { file, ... }: file;
      video = { file, ... }: file;
    };

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
