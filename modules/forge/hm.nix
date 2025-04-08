{ config, lib, ... }:

{
  options.stylix.targets.forge.enable =
    config.lib.stylix.mkEnableTarget "Forge" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.forge.enable) {
    xdg.configFile."forge/stylesheet/forge/stylesheet.css".source =
      config.lib.stylix.colors
        {
          template = ./stylesheet.css.mustache;
          extension = ".css";
        };
  };
}
