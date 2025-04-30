{
  config,
  lib,
  ...
}:
let
  blenderVersions = [
    "blender/4.2/scripts/presets/interface_theme"
    "blender/4.3/scripts/presets/interface_theme"
    "blender/4.4/scripts/presets/interface_theme"
  ];
in
{
  options.stylix.targets.blender.enable =
    config.lib.stylix.mkEnableTarget "blender" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.blender.enable)
      {
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
            builtins.foldl' (
              acc: version:
              acc
              // {
                "blender/${version}/scripts/presets/interface_theme/Stylix.xml".text =
                  builtins.replaceStrings
                    [ "%POPUPSFONTSIZE%" "%DESKTOPFONTSIZE%" ]
                    [
                      (toString config.stylix.fonts.sizes.popups)
                      (toString config.stylix.fonts.sizes.desktop)
                    ]
                    theme;
              }
            ) { } versions;
        }
      };
}
