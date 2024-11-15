{ config, lib, ... }:

with config.lib.stylix;
{
  options.stylix.targets.hyprlock.enable = mkEnableTarget "Hyprlock" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.hyprlock.enable)
      {
        programs.hyprlock.settings = {
          background.path = "${config.stylix.image}";
          input-field = with colors; {
            outer_color = "rgb(${base03})";
            inner_color = "rgb(${base00})";
            font_color = "rgb(${base05})";
            fail_color = "rgb(${base08})";
            check_color = "rgb(${base0A})";
          };
        };
      };
}
