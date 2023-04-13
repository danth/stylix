{ pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  rofiOpacity = builtins.toString (builtins.ceil (config.stylix.opacity.popups * 100));
  finalString = ''
   * { background: rgba ( {{base00-rgb-r}}, {{base00-rgb-g}}, {{base00-rgb-b}}, ${rofiOpacity} % );
       lightbg: rgba ( {{base01-rgb-r}}, {{base01-rgb-g}}, {{base01-rgb-b}}, ${rofiOpacity} % );
   }
   '' + builtins.toString (builtins.readFile ./template.mustache);
  finalFile = config.lib.stylix.colors {
      template = finalString;
      extension = ".rasi";
  };
in
{
  options.stylix.targets.rofi.enable =
    config.lib.stylix.mkEnableTarget "Rofi" true;

  config = lib.mkIf config.stylix.targets.rofi.enable {
    programs.rofi = {
      font = "${monospace.name} ${toString sizes.popups}";
      theme = finalFile;
    };
  };
}
