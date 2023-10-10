{ pkgs, config, lib, ... }:

with config.stylix.fonts;

let
  themeFile = config.stylix.colors {
    templateRepo = config.lib.stylix.templates.base16-xresources;
  };

in
{
  options.stylix.targets.xresources.enable =
    config.lib.stylix.mkEnableTarget "Xresources" true;

  config = lib.mkIf config.stylix.targets.xresources.enable {
    xresources = {
      properties = {
        "*.faceName" = monospace.name;
        "*.faceSize" = sizes.terminal;
        "*.renderFont" = true;
      };
      extraConfig = builtins.readFile themeFile;
    };
  };
}
