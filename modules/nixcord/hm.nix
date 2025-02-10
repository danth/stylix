{
  config,
  lib,
  options,
  ...
}:
let
  template =
    let
      inherit (config.lib.stylix) colors;
      inherit (config.stylix) fonts;
    in
    import ../vencord/template.nix { inherit colors fonts; };
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
                "Vencord/themes/stylix.theme.css".text = template;
              })

              (lib.mkIf nixcord.vesktop.enable {
                "vesktop/themes/stylix.theme.css".text = template;
              })
            ];

          programs.nixcord.config.enabledThemes = [ "stylix.theme.css" ];
        }
      );
}
