{ config, lib, ... }:

let
  themeFile = config.lib.stylix.colors {
    template = ./template.mustache;
    extension = ".toml";
  };
in
{
  options.stylix.targets.halloy.enable =
    config.lib.stylix.mkEnableTarget "Halloy" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.halloy.enable)
      {
        xdg.configFile."halloy/themes/Stylix.toml".source = themeFile;
      };
}
