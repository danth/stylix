{ config, lib, ... }:
let
  template =
    let
      inherit (config.lib.stylix) colors;
      inherit (config.stylix) fonts;
    in
    import ./template.nix { inherit colors fonts; };
in
{
  options.stylix.targets.vencord.enable =
    config.lib.stylix.mkEnableTarget "Vencord" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.vencord.enable)
      {
        xdg.configFile."Vencord/themes/stylix.theme.css".text = template;
      };
}
