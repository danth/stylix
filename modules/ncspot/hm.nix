{ config, lib, ... }:

{
  options.stylix.targets.ncspot.enable =
    config.lib.stylix.mkEnableTarget "Ncspot" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.ncspot.enable)
      {
        programs.ncspot.settings =
          let
            colors = config.lib.stylix.colors.withHashtag;
          in
          {
            theme = with colors; {
              background =
                if (config.stylix.opacity.terminal != 1.0) then "#00000000" else base00;
              primary = base05;
              secondary = base04;
              title = base06;
              playing = base0B;
              playing_selected = base0B;
              playing_bg =
                if (config.stylix.opacity.terminal != 1.0) then "#00000000" else base00;
              highlight = base05;
              highlight_bg = base02;
              error = base05;
              error_bg = base08;
              statusbar = base00;
              statusbar_progress = base04;
              statusbar_bg = base04;
              cmdline = base02;
              cmdline_bg = base05;
              search_match = base05;
            };
          };
      };
}
