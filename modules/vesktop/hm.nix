{
  pkgs,
  config,
  lib,
  ...
}:
let
  themeFile = config.lib.stylix.colors {
    template = ../vencord/template.mustache;
    extension = ".css";
  };
in
{
  options.stylix.targets.vesktop.enable =
    config.lib.stylix.mkEnableTarget "Vesktop" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.vesktop.enable)
      (
        lib.mkMerge [
          (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
            xdg.configFile."vesktop/themes/stylix.theme.css".source = themeFile;
          })

          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
            home.file."Library/Application Support/vesktop/themes/stylix.theme.css".source =
              themeFile;
          })
        ]
      );
}
