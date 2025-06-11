{ mkTarget, lib, ... }:
mkTarget {
  name = "ncspot";
  humanName = "Ncspot";

  extraOptions.background = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    description = "Used to set bg even if `opacity` or `colors` is null.";
    internal = true;
    default = null;
  };

  configElements = [
    (
      { cfg }:
      lib.mkIf (cfg.background != null) {
        programs.ncspot.settings.theme = {
          inherit (cfg) background;
          playing_bg = cfg.background;
        };
      }
    )
    (
      { opacity }:
      {
        stylix.targets.ncspot.background = lib.mkIf (
          opacity.terminal != 1.0
        ) "#00000000";
      }
    )
    (
      { colors }:
      with colors.withHashtag;
      {
        stylix.targets.ncspot.background = lib.mkDefault base00;
        programs.ncspot.settings.theme = {
          primary = base05;
          secondary = base04;
          title = base06;
          playing = base0B;
          playing_selected = base0B;
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
      }
    )
  ];
}
