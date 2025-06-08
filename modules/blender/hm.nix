{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.stylix.targets.blender.enable =
    config.lib.stylix.mkEnableTarget "blender" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.blender.enable)
      {
        xdg.configFile =
          let
            theme = pkgs.writeText "Stylix.replaced.xml.mustache" (
              builtins.replaceStrings
                [ "%POPUPSFONTSIZE%" "%DESKTOPFONTSIZE%" ]
                [
                  (toString config.stylix.fonts.sizes.popups)
                  (toString config.stylix.fonts.sizes.desktop)
                ]
                (builtins.readFile ./Stylix.xml.mustache)
            );
          in
          builtins.listToAttrs (
            map
              (
                version:
                lib.nameValuePair
                  "blender/${version}/scripts/presets/interface_theme/Stylix.xml"
                  {
                    source = config.lib.stylix.colors {
                      template = theme;
                      extension = ".xml";
                    };
                  }
              )
              [
                "4.2"
                "4.3"
                "4.4"
              ]
          );
      };
}
