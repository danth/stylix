{ pkgs, config, lib, ... }:

let
  themeFile = config.lib.stylix.colors {
    template = builtins.readFile ./base16-stylix.mustache;
    extension = ".tmTheme";
  };
in
{
  options.stylix.targets.bat.enable =
    config.lib.stylix.mkEnableTarget "Bat" config.programs.bat.enable;

  config = lib.mkIf config.stylix.targets.bat.enable {
    programs.bat.themes."base16-stylix" = builtins.readFile themeFile;
    home.sessionVariables.BAT_THEME = "base16-stylix";
  };
}
