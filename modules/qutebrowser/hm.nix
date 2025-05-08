{ config, lib, ... }:
let
  colors = config.lib.stylix.colors.withHashtag;
  background = colors.base00;
  secondary-background = colors.base01;
  selection-background = colors.base03;

  foreground = colors.base05;
  inverted-foreground = colors.base00;

  error = colors.base08;

  info = colors.base0B;
  secondary-info = colors.base0C;

  warning = colors.base0E;
in
{
  options.stylix.targets.qutebrowser.enable =
    config.lib.stylix.mkEnableTarget "Qutebrowser" true;

  config =
    with config.stylix.fonts;
    lib.mkIf (config.stylix.enable && config.stylix.targets.qutebrowser.enable) {
      programs.qutebrowser.settings = {
        colors = {
          completion = {
            category = {
              bg = background;
              fg = info;

              border = {
                bottom = background;
                top = background;
              };
            };

            even.bg = background;
            fg = foreground;

            item.selected = {
              bg = selection-background;

              border = {
                bottom = selection-background;
                top = selection-background;
              };

              fg = foreground;
            };

            match.fg = info;
            odd.bg = secondary-background;

            scrollbar = {
              bg = background;
              fg = foreground;
            };
          };

          contextmenu = {
            disabled = {
              bg = secondary-background;
              fg = inverted-foreground;
            };

            menu = {
              bg = background;
              fg = foreground;
            };

            selected = {
              bg = selection-background;
              fg = foreground;
            };
          };

          downloads = {
            bar.bg = background;

            error = {
              bg = error;
              fg = inverted-foreground;
            };

            start = {
              bg = info;
              fg = inverted-foreground;
            };

            stop = {
              bg = secondary-info;
              fg = inverted-foreground;
            };
          };

          hints = {
            bg = secondary-background;
            fg = foreground;
            match.fg = info;
          };

          keyhint = {
            bg = background;
            fg = foreground;
            suffix.fg = foreground;
          };

          messages = {
            error = {
              bg = error;
              fg = inverted-foreground;
              border = error;
            };

            info = {
              bg = info;
              fg = inverted-foreground;
              border = info;
            };

            warning = {
              bg = warning;
              fg = inverted-foreground;
              border = warning;
            };
          };

          prompts = {
            bg = background;
            border = background;
            fg = foreground;
            selected.bg = secondary-background;
          };

          statusbar = {
            caret = {
              bg = selection-background;
              fg = foreground;

              selection = {
                bg = selection-background;
                fg = foreground;
              };
            };

            command = {
              bg = background;
              fg = foreground;

              private = {
                bg = secondary-background;
                fg = foreground;
              };
            };

            insert = {
              bg = info;
              fg = inverted-foreground;
            };

            normal = {
              bg = background;
              fg = foreground;
            };

            passthrough = {
              bg = secondary-info;
              fg = inverted-foreground;
            };

            private = {
              bg = secondary-background;
              fg = foreground;
            };

            progress.bg = info;

            url = {
              error.fg = error;
              fg = foreground;
              hover.fg = foreground;

              success = {
                http.fg = secondary-info;
                https.fg = info;
              };

              warn.fg = warning;
            };
          };

          tabs = {
            bar.bg = background;

            even = {
              bg = secondary-background;
              fg = foreground;
            };

            indicator = {
              inherit error;
              start = secondary-info;
              stop = info;
            };

            odd = {
              bg = background;
              fg = foreground;
            };

            pinned = {
              even = {
                bg = info;
                fg = inverted-foreground;
              };

              odd = {
                bg = secondary-info;
                fg = inverted-foreground;
              };

              selected = {
                even = {
                  bg = selection-background;
                  fg = foreground;
                };

                odd = {
                  bg = selection-background;
                  fg = foreground;
                };
              };
            };

            selected = {
              even = {
                bg = selection-background;
                fg = foreground;
              };

              odd = {
                bg = selection-background;
                fg = foreground;
              };
            };
          };

          webpage.preferred_color_scheme = lib.mkIf (config.stylix.polarity == "dark") (
            lib.mkDefault config.stylix.polarity
          );
        };

        fonts = {
          default_family = sansSerif.name;
          default_size = "${toString sizes.applications}pt";

          web = {
            family = {
              cursive = serif.name;
              fantasy = serif.name;
              fixed = monospace.name;
              sans_serif = sansSerif.name;
              serif = serif.name;
              standard = sansSerif.name;
            };

            # TODO: Use the pixel unit:
            # https://github.com/danth/stylix/issues/251.
            size.default = builtins.floor (sizes.applications * 4 / 3 + 0.5);
          };
        };

        hints.border = background;
      };
    };
}
