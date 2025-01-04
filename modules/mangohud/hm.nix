{ config, lib, ... }:

let
  inherit (config.stylix) fonts opacity;
  inherit (config.lib.stylix) colors;
in
{
  options.stylix.targets.mangohud.enable =
    config.lib.stylix.mkEnableTarget "mangohud" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.mangohud.enable)
      {
        programs.mangohud.settings = with colors; {
          font_size = fonts.sizes.applications;
          font_size_text = fonts.sizes.applications;
          background_alpha = opacity.popups;
          alpha = opacity.applications;
          text_color = base05;
          text_outline_color = base00;
          background_color = base00;
          gpu_color = base0B;
          cpu_color = base0D;
          vram_color = base0C;
          media_player_color = base05;
          engine_color = base0E;
          wine_color = base0E;
          frametime_color = base0B;
          battery_color = base04;
          io_color = base0A;
          gpu_load_color = "${base0B}, ${base0A}, ${base08}";
          cpu_load_color = "${base0B}, ${base0A}, ${base08}";
          fps_color = "${base0B}, ${base0A}, ${base08}";

          # TODO: Use the point unit:
          # https://github.com/danth/stylix/issues/251.
          font_scale = 1.33333;
        };
      };

}
