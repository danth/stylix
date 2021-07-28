{ config, ... }:

with config.lib.stylix.colors;

let
  text = base05-hash;
  urgent = base08-hash;
  focused = base0A-hash;
  unfocused = base03-hash;

  fonts = {
    names = [ config.stylix.fonts.sansSerif.name ];
    size = 8.0;
  };

in {
  stylix.homeModule = {
    wayland.windowManager.sway.config = {
      inherit fonts;

      colors = let
        background = base00-hash;
        indicator = base0B-hash;
      in {
        inherit background;
        urgent = {
          inherit background indicator text;
          border = urgent;
          childBorder = urgent;
        };
        focused = {
          inherit background indicator text;
          border = focused;
          childBorder = focused;
        };
        focusedInactive = {
          inherit background indicator text;
          border = unfocused;
          childBorder = unfocused;
        };
        unfocused = {
          inherit background indicator text;
          border = unfocused;
          childBorder = unfocused;
        };
        placeholder = {
          inherit background indicator text;
          border = unfocused;
          childBorder = unfocused;
        };
      };

      output."*".bg = "${config.stylix.image} fill";
    };
  };

  # Merge this with your bar configuration using //config.lib.stylix.sway.bar
  lib.stylix.sway.bar = {
    inherit fonts;

    colors = let
      background = base01-hash;
      border = background;
    in {
      inherit background;
      statusline = text;
      separator = base03-hash;
      focusedWorkspace = {
        inherit text border;
        background = focused;
      };
      activeWorkspace = {
        inherit text border;
        background = unfocused;
      };
      inactiveWorkspace = {
        inherit text border;
        background = unfocused;
      };
      urgentWorkspace = {
        inherit text border;
        background = urgent;
      };
      bindingMode = {
        inherit text border;
        background = urgent;
      };
    };
  };
}
