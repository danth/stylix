{
  pkgs,
  config,
  lib,
  ...
}:
let
  themeText =
    builtins.readFile (
      config.lib.stylix.colors {
        template = ../vencord/template.mustache;
        extension = ".css";
      }
    )
    + ''
      :root {
          --font-primary: ${config.stylix.fonts.sansSerif.name};
          --font-display: ${config.stylix.fonts.sansSerif.name};
          --font-code: ${config.stylix.fonts.monospace.name};
      }
    '';
in
{
  options.stylix.targets.vesktop.enable =
    config.lib.stylix.mkEnableTarget "Vesktop" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.vesktop.enable)
      (
        lib.mkMerge [
          (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
            xdg.configFile."vesktop/themes/stylix.theme.css".text = themeText;
          })

          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
            home.file."Library/Application Support/vesktop/themes/stylix.theme.css".text =
              themeText;
          })
        ]
      );
}
