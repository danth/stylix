{ config, lib, ... }:
let
  themeFile = config.lib.stylix.colors {
    template = ../vesktop/template.mustache;
    extension = ".css";
  };
in
{
  options.stylix.targets.nixcord.enable =
    config.lib.stylix.mkEnableTarget "Nixcord" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.nixcord.enable)
      {
        xdg.configFile."Vencord/themes/stylix.theme.css".source = themeFile;
        programs.nixcord.config.enabledThemes = [ "stylix.theme.css" ];
      };
}
