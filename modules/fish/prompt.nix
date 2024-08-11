config:

let
  theme = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.base16-fish;
  };
in ''
  source ${theme}

  if test -n "$base16_theme" && status --is-interactive && test -z "$TMUX"
    base16-${config.lib.stylix.colors.slug}
  end
''
