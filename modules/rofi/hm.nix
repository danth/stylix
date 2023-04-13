{ pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  rofiOpacity = builtins.toString (builtins.ceil (config.stylix.opacity.popups * 100));
  finalString = builtins.replaceStrings [
   "background: rgba ( {{base00-rgb-r}}, {{base00-rgb-g}}, {{base00-rgb-b}}, 100 % );"
   "lightbg: rgba ( {{base01-rgb-r}}, {{base01-rgb-g}}, {{base01-rgb-b}}, 100 % );"
  ] [
   "background: rgba ( {{base00-rgb-r}}, {{base00-rgb-g}}, {{base00-rgb-b}}, ${rofiOpacity} % );"
   "lightbg: rgba ( {{base01-rgb-r}}, {{base01-rgb-g}}, {{base01-rgb-b}}, ${rofiOpacity} % );"
  ] (builtins.readFile ./templates.mustache);
  finalFile = config.lib.stylix.colors {
      templateRepo = builtins.toFile "Final" finalString;
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
