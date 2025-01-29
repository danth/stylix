{
  config,
  lib,
  options,
  ...
}:
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
      (
        lib.optionalAttrs (builtins.hasAttr "nixcord" options.programs) {
          xdg.configFile =
            let
              inherit (config.programs) nixcord;
            in
            lib.mkMerge [
              (lib.mkIf nixcord.discord.enable {
                "Vencord/themes/stylix.theme.css".source = themeFile;
              })

              (lib.mkIf nixcord.vesktop.enable {
                "vesktop/themes/stylix.theme.css".source = themeFile;
              })
            ];

          programs.nixcord.config.enabledThemes = [ themeFileName ];
        }
      );
}
