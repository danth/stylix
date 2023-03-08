args:
{ config, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config = {
    xdg.configFile = {
      # See ../nixos/palette.nix for the rational behind these two options
      "stylix/generated.json".source = config.lib.stylix.scheme {
        template = builtins.readFile ../palette.json.mustache;
        extension = ".json";
      };
      "stylix/palette.json".source = config.lib.stylix.colors {
        template = builtins.readFile ../palette.json.mustache;
        extension = ".json";
      };
      "stylix/palette.html".source = config.lib.stylix.colors {
        template = builtins.readFile ../palette.html.mustache;
        extension = ".html";
      };
    };
  };
}
