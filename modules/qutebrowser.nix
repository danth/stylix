{ config, ... }:

with config.stylix.fonts;
with config.lib.stylix.colors;

let
  background = "#${base00-hex}";
  secondary-background = "#${base01-hex}";
  selection-background = "#${base03-hex}";
  foreground = "#${base05-hex}";
  inverted-foreground = "#${base00-hex}";
  info = "#${base0B-hex}";
  secondary-info = "#${base0C-hex}";
  warning = "#${base0E-hex}";
  error = "#${base08-hex}";

in {
  stylix.homeModule = {
    programs.qutebrowser.settings = {
      hints.border = background;
      colors = {
        completion = {
          fg = foreground;
          odd.bg = secondary-background;
          even.bg = background;
          match.fg = info;
          category = {
            fg = info;
            bg = background;
            border.top = background;
            border.bottom = background;
          };
          item.selected = {
            fg = foreground;
            bg = selection-background;
            border.top = selection-background;
            border.bottom = selection-background;
          };
          scrollbar = {
            fg = foreground;
            bg = background;
          };
        };
        contextmenu = {
          disabled = {
            fg = inverted-foreground;
            bg = secondary-background;
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
          start = {
            fg = inverted-foreground;
            bg = info;
          };
          stop = {
            fg = inverted-foreground;
            bg = secondary-info;
          };
          error = {
            fg = inverted-foreground;
            bg = error;
          };
        };
        hints = {
          fg = foreground;
          bg = secondary-background;
          match.fg = info;
        };
        keyhint = {
          fg = foreground;
          bg = background;
          suffix.fg = foreground;
        };
        messages = {
          error = {
            fg = inverted-foreground;
            bg = error;
            border = error;
          };
          warning = {
            fg = inverted-foreground;
            bg = warning;
            border = warning;
          };
          info = {
            fg = inverted-foreground;
            bg = info;
            border = info;
          };
        };
        prompts = {
          fg = foreground;
          bg = background;
          border = background;
          selected.bg = secondary-background;
        };
        statusbar = {
          normal = {
            fg = foreground;
            bg = background;
          };
          insert = {
            fg = inverted-foreground;
            bg = info;
          };
          passthrough = {
            fg = inverted-foreground;
            bg = secondary-info;
          };
          private = {
            fg = foreground;
            bg = secondary-background;
          };
          command = {
            fg = foreground;
            bg = background;
            private = {
              fg = foreground;
              bg = secondary-background;
            };
          };
          caret = {
            fg = foreground;
            bg = selection-background;
            selection = {
              fg = foreground;
              bg = selection-background;
            };
          };
          progress.bg = info;
          url = {
            fg = foreground;
            error.fg = error;
            hover.fg = foreground;
            success.http.fg = secondary-info;
            success.https.fg = info;
            warn.fg = warning;
          };
        };
        tabs = {
          bar.bg = background;
          indicator = {
            start = secondary-info;
            stop = info;
            error = error;
          };
          odd = {
            fg = foreground;
            bg = background;
          };
          even = {
            fg = foreground;
            bg = secondary-background;
          };
          pinned = {
            even = {
              fg = inverted-foreground;
              bg = info;
            };
            odd = {
              fg = inverted-foreground;
              bg = secondary-info;
            };
            selected = {
              even = {
                fg = foreground;
                bg = selection-background;
              };
              odd = {
                fg = foreground;
                bg = selection-background;
              };
            };
          };
          selected = {
            even = {
              fg = foreground;
              bg = selection-background;
            };
            odd = {
              fg = foreground;
              bg = selection-background;
            };
          };
        };
      };

      fonts = {
        completion = {
          category = sansSerif.name;
          entry = sansSerif.name;
        };
        contextmenu = sansSerif.name;
        debug_console = monospace.name;
        default_family = sansSerif.name;
        downloads = sansSerif.name;
        hints = monospace.name;
        keyhint = monospace.name;
        messages = {
          error = sansSerif.name;
          info = sansSerif.name;
          warning = sansSerif.name;
        };
        prompts = sansSerif.name;
        statusbar = sansSerif.name;
        tabs = {
          selected = sansSerif.name;
          unselected = sansSerif.name;
        };
        web.family = {
          cursive = serif.name;
          fantasy = serif.name;
          fixed = monospace.name;
          sans_serif = sansSerif.name;
          serif = serif.name;
          standard = sansSerif.name;
        };
      };
    };
  };
}
