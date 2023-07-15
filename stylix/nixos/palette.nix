args:
{ config, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config = {
    environment.etc = {
      # Making palette.json part of the system closure will protect it from
      # garbage collection, so future configurations can be evaluated without
      # having to generate the palette again. The generator is not kept, only
      # the palette which came from it, so this uses very little disk space.
      # The extra indirection should prevent the palette generator from running
      # when the theme is manually specified. generated.json is necessary in
      # the presence of overrides.
      "stylix/generated.json".source = config.lib.stylix.scheme {
        template = ../palette.json.mustache;
        extension = ".json";
      };

      "stylix/palette.json".source = config.lib.stylix.colors {
        template = ../palette.json.mustache;
        extension = ".json";
      };

      # We also provide a HTML version which is useful for viewing the colors
      # during development.
      "stylix/palette.html".source = config.lib.stylix.colors {
        template = ../palette.html.mustache;
        extension = ".html";
      };
    };
  };
}
