{
  config,
  lib,
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
            theme = builtins.readFile (
              config.lib.stylix.colors {
                template = ./Stylix.xml.mustache;
                extension = ".xml";
              }
            );
          in
          builtins.listToAttrs (
            map
              (
                version:
                lib.nameValuePair
                  "blender/${version}/scripts/presets/interface_theme/Stylix.xml"
                  {
                    text =
                      builtins.replaceStrings
                        [ "%POPUPSFONTSIZE%" "%DESKTOPFONTSIZE%" ]
                        [
                          (toString config.stylix.fonts.sizes.popups)
                          (toString config.stylix.fonts.sizes.desktop)
                        ]
                        theme;
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
