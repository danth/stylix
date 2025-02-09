{ config, lib, ... }:

with config.lib.stylix;

let
  cfg = config.stylix.targets.hyprlock;
in
{
  options.stylix.targets.hyprlock = {
    enable = mkEnableTarget "Hyprlock" true;
    useWallpaper = mkEnableWallpaper "Hyprlock" true;
  };

  config = lib.mkIf (config.stylix.enable && cfg.enable) {
    programs.hyprlock.settings = {
      background = {
        path = lib.mkIf cfg.useWallpaper config.stylix.image;
        color = "rgb(${base00})";
      };
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
