{
  mkTarget,
  config,
  lib,
  ...
}:
mkTarget {
  name = "hyprlock";
  humanName = "Hyprlock";

  extraOptions = {
    useWallpaper = config.lib.stylix.mkEnableWallpaper "Hyprlock" true;
  };

  configElements = [
    (
      { cfg, image }:
      {
        programs.hyprlock.settings.background.path = lib.mkIf cfg.useWallpaper "${
          image
        }";
      }
    )
    (
      { colors }:
      {
        programs.hyprlock.settings = with colors; {
          background = {
            color = "rgb(${base00})";
          };
          input-field = {
            outer_color = "rgb(${base03})";
            inner_color = "rgb(${base00})";
            font_color = "rgb(${base05})";
            fail_color = "rgb(${base08})";
            check_color = "rgb(${base0A})";
          };
        };
      }
    )
  ];
}
