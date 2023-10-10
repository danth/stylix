{ pkgs, config }:

let
  theme = config.stylix.colors {
    templateRepo = config.lib.stylix.templates.base16-fish;
  };

in ''
  source ${theme}
  base16-${config.stylix.colors.slug}
''
