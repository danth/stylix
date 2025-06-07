{ mkTarget, lib, ... }:
mkTarget {
  name = "i3status-rust";
  humanName = "i3status-rust";

  # Merge this with your bar's theme's overrides with //config.lib.stylix.i3status-rust.bar
  generalConfig =
    { colors }:
    {
      lib.stylix.i3status-rust.bar = lib.mkIf (colors != null) (
        with colors.withHashtag;
        {
          idle_bg = base00;
          idle_fg = base05;
          info_bg = base09;
          info_fg = base00;
          good_bg = base01;
          good_fg = base05;
          warning_bg = base0A;
          warning_fg = base00;
          critical_bg = base08;
          critical_fg = base00;
          separator_bg = base00;
          separator_fg = base05;
        }
      );
    };
}
