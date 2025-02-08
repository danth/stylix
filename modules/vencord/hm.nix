{ config, lib, ... }:
let
  themeText =
    builtins.readFile (
      config.lib.stylix.colors {
        template = ./template.mustache;
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
  options.stylix.targets.vencord.enable =
    config.lib.stylix.mkEnableTarget "Vencord" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.vencord.enable)
      {
        xdg.configFile."Vencord/themes/stylix.theme.css".text = themeText;
      };
}
