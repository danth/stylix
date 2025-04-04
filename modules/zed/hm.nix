{
  config,
  lib,
  ...
}:
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
        programs.zed-editor.userSettings =
          let
            inherit (config.stylix) fonts;
          in
          {
            "buffer_font_family" = fonts.monospace.name;
            "buffer_font_size" = fonts.sizes.terminal * 4.0 / 3.0;
            "theme" = "Base16 ${config.lib.stylix.colors.scheme-name}";
            "ui_font_family" = fonts.sansSerif.name;
            "ui_font_size" = fonts.sizes.applications * 4.0 / 3.0;
          };

        xdg.configFile."zed/themes/nix.json".source = config.lib.stylix.colors {
          templateRepo = config.stylix.inputs.tinted-zed;
        };
      };
}
