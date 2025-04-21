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
        xdg.configFile = builtins.listToAttrs (
          builtins.concatMap (version: [
            {
              name = "${version}/Stylix.xml";
              value.text =
                let
                  coloredFile = config.lib.stylix.colors {
                    template = ./Stylix.xml.mustache;
                    extension = ".xml";
                  };
                in
                builtins.replaceStrings
                  [ "%POPUPSFONTSIZE%" "%DESKTOPFONTSIZE%" ]
                  [
                    (toString config.stylix.fonts.sizes.popups)
                    (toString config.stylix.fonts.sizes.desktop)
                  ]
                  (builtins.readFile coloredFile);
            }
          ]) blenderVersions
        );
      };
}
