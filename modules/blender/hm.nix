{
  mkTarget,
  lib,
  ...
}:
mkTarget {
  name = "blender";
  humanName = "Blender";

  extraOptions.themeBody = lib.mkOption {
    type = lib.types.lines;
    default = "";
    internal = true;
  };

  configElements = [
    (import ./color-theme.nix)
    (import ./font-theme.nix)
    (
      { cfg }:
      {
        xdg.configFile =
          let
            indent =
              string: "  " + lib.concatStringsSep "\n  " (lib.splitString "\n" string);
          in
          lib.mkIf (cfg.themeBody != "") (
            builtins.listToAttrs (
              map
                (
                  version:
                  lib.nameValuePair
                    "blender/${version}/scripts/presets/interface_theme/Stylix.xml"
                    {
                      text = lib.concatLines [
                        "<bpy>"
                        (indent cfg.themeBody)
                        "</bpy>"
                      ];
                    }
                )
                [
                  "4.2"
                  "4.3"
                  "4.4"
                ]
            )
          );
      }
    )
  ];
}
