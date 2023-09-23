args:
{ config, lib, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config.xdg.configFile = {
    "stylix/palette.json".source = config.lib.stylix.colors {
      template = ../palette.json.mustache;
      extension = ".json";
    };
    "stylix/palette.html".source = config.lib.stylix.colors {
      template = ../palette.html.mustache;
      extension = ".html";
    };
  };
}
