{
  config,
  lib,
  ...
}:
{
  options.stylix.targets.spotify-player.enable =
    config.lib.stylix.mkEnableTarget "spotify-player" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.spotify-player.enable)
      {
        programs.spotify-player.settings.theme = "stylix";
        programs.spotify-player.themes = [
          {
            name = "stylix";
            palette = {
              background = "#${config.lib.stylix.colors.base00}";
              foreground = "#${config.lib.stylix.colors.base05}";
              black = "#${config.lib.stylix.colors.base00}";
              red = "#${config.lib.stylix.colors.base08}";
              green = "#${config.lib.stylix.colors.base0B}";
              yellow = "#${config.lib.stylix.colors.base0A}";
              blue = "#${config.lib.stylix.colors.base0D}";
              magenta = "#${config.lib.stylix.colors.base0E}";
              cyan = "#${config.lib.stylix.colors.base0C}";
              white = "#${config.lib.stylix.colors.base05}";
              bright_black = "#${config.lib.stylix.colors.base03}";
              bright_red = "#${config.lib.stylix.colors.base08}";
              bright_green = "#${config.lib.stylix.colors.base0B}";
              bright_yellow = "#${config.lib.stylix.colors.base0A}";
              bright_blue = "#${config.lib.stylix.colors.base0D}";
              bright_magenta = "#${config.lib.stylix.colors.base0E}";
              bright_cyan = "#${config.lib.stylix.colors.base0C}";
              bright_white = "#${config.lib.stylix.colors.base07}";
            };
          }
        ];
      };
}
