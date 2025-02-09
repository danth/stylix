{
  pkgs,
  config,
  lib,
  ...
}:
let
  template =
    let
      inherit (config.lib.stylix) colors;
      inherit (config.stylix) fonts;
    in
    import ../vencord/template.nix { inherit colors fonts; };
in
{
  options.stylix.targets.vesktop.enable =
    config.lib.stylix.mkEnableTarget "Vesktop" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.vesktop.enable)
      (
        lib.mkMerge [
          (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
            xdg.configFile."vesktop/themes/stylix.theme.css".text = template;
          })

          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
            home.file."Library/Application Support/vesktop/themes/stylix.theme.css".text =
              template;
          })
        ]
      );
}
