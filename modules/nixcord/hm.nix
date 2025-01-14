{ config, lib, ... }:
let
  themeFile = config.lib.stylix.colors {
    template = ../vencord/template.mustache;
    extension = ".css";
  };
  themeFileName = "stylix.theme.css";
  cfg = config.stylix.targets.nixcord;
in
{
  options.stylix.targets.nixcord.enable =
    config.lib.stylix.mkEnableTarget "Nixcord" true;

  config =
    lib.mkIf (config.stylix.enable && cfg.enable && (config.programs ? nixcord))
      {
        xdg.configFile."Vencord/themes/stylix.theme.css".source = themeFile;
        programs.nixcord.config.enabledThemes = [ themeFileName ];
      };
}
