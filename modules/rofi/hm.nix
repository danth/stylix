{ pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  rofiOpacity = builtins.toString (builtins.ceil (config.stylix.opacity.popups * 100));
  finalString = ''
   * { background: rgba ( {{base00-rgb-r}}, {{base00-rgb-g}}, {{base00-rgb-b}}, ${rofiOpacity} % );
       lightbg: rgba ( {{base01-rgb-r}}, {{base01-rgb-g}}, {{base01-rgb-b}}, ${rofiOpacity} % );
       red: rgba ( {{base08-rgb-r}}, {{base08-rgb-g}}, {{base08-rgb-b}}, ${rofiOpacity} % );
       blue: rgba ( {{base0D-rgb-r}}, {{base0D-rgb-g}}, {{base0D-rgb-b}}, ${rofiOpacity} % );
       lightfg: rgba ( {{base06-rgb-r}}, {{base06-rgb-g}}, {{base06-rgb-b}}, ${rofiOpacity} % );
       foreground: rgba ( {{base05-rgb-r}}, {{base05-rgb-g}}, {{base05-rgb-b}}, ${rofiOpacity} % );
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
