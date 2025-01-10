{
  config,
  lib,
  ...
}:
let
  theme = config.lib.stylix.colors {
    templateRepo = config.lib.stylix.templates.tinted-zed;
  };
in
{
  options.stylix.targets.zed.enable = config.lib.stylix.mkEnableTarget "zed" true;

  config =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.zed.enable
        && config.programs.zed-editor.enable
      )
      {
        programs.zed-editor.userSettings = {
          "buffer_font_family" = config.stylix.fonts.monospace.name;
          "buffer_font_size" = config.stylix.fonts.sizes.terminal * 4.0 / 3.0;
          "theme" = "Base16 ${config.lib.stylix.colors.scheme-name}";
          "ui_font_family" = config.stylix.fonts.sansSerif.name;
          "ui_font_size" = config.stylix.fonts.sizes.applications * 4.0 / 3.0;
        };

        xdg.configFile."zed/themes/nix.json".source = theme;
      };
}
