{ config, lib, ... }:
let
  cfg = config.stylix.targets.hyprlock;
in
{
  options.stylix.targets.hyprlock = with config.lib.stylix; {
    enable = mkEnableTarget "Hyprlock" true;
    useWallpaper = mkEnableWallpaper "Hyprlock" true;
  };

  config = lib.mkIf (config.stylix.enable && cfg.enable) {
    programs.hyprlock.settings = with config.lib.stylix.colors; {
      background = {
        color = "rgb(${base00})";
        path = lib.mkIf cfg.useWallpaper "${config.stylix.image}";
      };
      input-field = {
        outer_color = "rgb(${base03})";
        inner_color = "rgb(${base00})";
        font_color = "rgb(${base05})";
        fail_color = "rgb(${base08})";
        check_color = "rgb(${base0A})";
      };
    };
  };
}
