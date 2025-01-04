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
              background = base00;
              primary = base05;
              secondary = base03;
              title = base06;
              playing = base05;
              playing_selected = base06;
              playing_bg = base02;
              highlight = base05;
              highlight_bg = base02;
              error = base07;
              error_bg = base0F;
              statusbar = base00;
              statusbar_progress = base04;
              statusbar_bg = base04;
              cmdline = base05;
              cmdline_bg = base00;
              search_match = base05;
            };
          };
      };
}
