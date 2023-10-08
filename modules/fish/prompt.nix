{ pkgs, config }:

let
  theme = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.base16-fish;
  };
in ''
  source ${theme}
  base16-${config.lib.stylix.colors.slug}
''
