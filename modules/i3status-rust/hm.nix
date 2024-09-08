{ config, lib, ... }:

let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  # Merge this with your bar's theme's overrides with //config.lib.stylix.i3status-rust.bar
  config.lib.stylix.i3status-rust.bar = with colors; {
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
  };
}
