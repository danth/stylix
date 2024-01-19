{config, lib, ...}: {
  options.stylix.targets.taskwarrior.enable =
    config.lib.stylix.mkEnableTarget "Taskwarrior" true;

  config = let
    themeDir = "task";
    themeFile = "${themeName}.theme";
    themeName = "base16";
  in lib.mkIf config.stylix.targets.taskwarrior.enable {
    programs.taskwarrior.colorTheme = "${config.xdg.configHome}/${themeDir}/${themeName}";

    xdg.configFile."${themeDir}/${themeFile}".source = config.lib.stylix.colors {
      extension = "theme";
      template = ./template.theme.mustache;
    };
  };
}
