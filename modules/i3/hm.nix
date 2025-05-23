{ mkTarget, ... }:
mkTarget {
  name = "i3";
  humanName = "i3";

  configElements = [
    (
      { fonts }:
      {
        xsession.windowManager.i3.config.fonts = {
          names = [ fonts.sansSerif.name ];
          size = fonts.size.desktop * 1.0;
        };
        lib.stylix.i3.bar.fonts = {
          names = [ fonts.sansSerif.name ];
          size = fonts.size.desktop * 1.0;
        };
      }
    )
    (
      { colors }:
      with colors.withHashtag;
      let
        text = base05;
        urgent = base08;
        focused = base0D;
        unfocused = base03;
      in
      {
        xsession.windowManager.i3.config = {
          colors =
            let
              background = base00;
              indicator = base0B;
            in
            {
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

          #        output."*".bg = "${config.stylix.image} fill";
        };

        # Merge this with your bar configuration using //config.lib.stylix.i3.bar
        lib.stylix.i3.bar = {
          inherit fonts;

          colors =
            let
              background = base00;
              border = background;
            in
            {
              inherit background;
              statusline = text;
              separator = base03;
              focusedWorkspace = {
                inherit text background;
                border = focused;
              };
              activeWorkspace = {
                inherit border background;
                text = focused;
              };
              inactiveWorkspace = {
                inherit text border background;
              };
              urgentWorkspace = {
                inherit text background;
                border = urgent;
              };
              bindingMode = {
                inherit text border;
                background = urgent;
              };
            };
        };
      }
    )
  ];
}
