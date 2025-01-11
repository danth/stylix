{ config, lib, ... }:
let
  themeFile = config.lib.stylix.colors {
    template = ./template.mustache;
    extension = ".css";
  };
in
{
  options.stylix.targets.vencord.enable =
    config.lib.stylix.mkEnableTarget "Vencord" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.vencord.enable)
      {
        xdg.configFile."Vencord/themes/stylix.theme.css".source = themeFile;
      };
}
