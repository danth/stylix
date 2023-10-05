{ pkgs, config, lib, ... }:

{
  options.stylix.targets.bat.enable =
    config.lib.stylix.mkEnableTarget "Bat" config.programs.bat.enable;

  config = lib.mkIf config.stylix.targets.bat.enable {
    programs.bat.themes."base16-stylix".src = config.lib.stylix.colors {
      template = ./base16-stylix.mustache;
      extension = ".tmTheme";
    };
    home.sessionVariables.BAT_THEME = "base16-stylix";
  };
}
