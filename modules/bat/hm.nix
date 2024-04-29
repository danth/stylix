{ pkgs, config, lib, ... }:

{
  options.stylix.targets.bat.enable =
    config.lib.stylix.mkEnableTarget "Bat" config.programs.bat.enable;

  config = lib.mkIf config.stylix.targets.bat.enable {
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
