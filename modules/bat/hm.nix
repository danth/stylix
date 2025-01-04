{ config, lib, ... }:

{
  options.stylix.targets.bat.enable = config.lib.stylix.mkEnableTarget "Bat" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.bat.enable) {
    programs.bat = {
      # This theme is reused for yazi. Changes to the template
      # will need to be applied to modules/yazi/hm.nix
      themes."base16-stylix".src = config.lib.stylix.colors {
        template = ./base16-stylix.mustache;
        extension = ".tmTheme";
      };

      config.theme = "base16-stylix";
    };
  };
}
