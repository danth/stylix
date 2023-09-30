{ pkgs, config, lib, ... }:
with config.stylix.fonts;
let
  finalString = ''
   * { background: rgba ( {{base00-rgb-r}}, {{base00-rgb-g}}, {{base00-rgb-b}}, ${config.lib.stylix.popupsOpacity-int} % );
       lightbg: rgba ( {{base01-rgb-r}}, {{base01-rgb-g}}, {{base01-rgb-b}}, ${config.lib.stylix.popupsOpacity-int} % );
       red: rgba ( {{base08-rgb-r}}, {{base08-rgb-g}}, {{base08-rgb-b}}, ${config.lib.stylix.popupsOpacity-int} % );
       blue: rgba ( {{base0D-rgb-r}}, {{base0D-rgb-g}}, {{base0D-rgb-b}}, ${config.lib.stylix.popupsOpacity-int} % );
       lightfg: rgba ( {{base06-rgb-r}}, {{base06-rgb-g}}, {{base06-rgb-b}}, ${config.lib.stylix.popupsOpacity-int} % );
       foreground: rgba ( {{base05-rgb-r}}, {{base05-rgb-g}}, {{base05-rgb-b}}, ${config.lib.stylix.popupsOpacity-int} % );
   }
   '' + builtins.toString (builtins.readFile ./template.mustache);
  finalFile = config.stylix.colors {
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
