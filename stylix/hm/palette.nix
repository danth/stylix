args:
{ config, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config = {
    xdg.configFile = {
      # See ../nixos/palette.nix for the rational behind these two options
      "stylix/palette.json".source = config.stylix.generatedJSON;
      "stylix/palette.html".source = config.lib.stylix.colors {
        template = builtins.readFile ../palette.html.mustache;
        extension = ".html";
      };
    };
  };
}
