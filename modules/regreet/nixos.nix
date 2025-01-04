{
  pkgs,
  config,
  lib,
  ...
}:

{
  options.stylix.targets.regreet.enable =
    config.lib.stylix.mkEnableTarget "ReGreet" true;

  config =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.regreet.enable
        && pkgs.stdenv.hostPlatform.isLinux
      )
      {
        programs.regreet = {
          settings.background = {
            path = config.stylix.image;
            fit =
              let
                inherit (config.stylix) imageScalingMode;
              in
              if imageScalingMode == "fill" then
                "Cover"
              else if imageScalingMode == "fit" then
                "Contain"
              else if imageScalingMode == "stretch" then
                "Fill"
              # No other available options
              else
                null;
          };
          font = {
            inherit (config.stylix.fonts.sansSerif) name package;
          };
          cursorTheme = {
            inherit (config.stylix.cursor) name package;
          };
          theme = {
            package = pkgs.adw-gtk3;
            name = "adw-gtk3";
          };
          extraCss = config.lib.stylix.colors {
            template = ./../gtk/gtk.mustache;
            extension = "css";
          };
        };
      };
}
