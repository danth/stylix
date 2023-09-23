args:
{ config, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config.environment.etc = {
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
