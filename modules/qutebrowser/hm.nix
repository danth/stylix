{ config, lib, ... }:

with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;

let
  background = base00;
  error = base08;
  foreground = base05;
  info = base0B;
  inverted-foreground = base00;
  secondary-background = base01;
  secondary-info = base0C;
  selection-background = base03;
  warning = base0E;
in {
  options.stylix.targets.qutebrowser.enable =
    config.lib.stylix.mkEnableTarget "Qutebrowser" true;

  config = lib.mkIf config.stylix.targets.qutebrowser.enable {
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
            error = error;
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
        webpage = let
          isDark = config.stylix.polarity == "dark";
        in {
          darkmode.enabled = lib.mkIf isDark (lib.mkDefault true);
          preferred_color_scheme =
            lib.mkIf
            isDark (lib.mkDefault config.stylix.polarity);
        };
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
          size.default = "${toString sizes.applications}pt";
        };
      };

      hints.border = background;
    };
  };
}
