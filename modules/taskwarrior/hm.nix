{config, lib, ...}: {
  options.stylix.targets.taskwarrior.enable =
    config.lib.stylix.mkEnableTarget "Taskwarrior" true;

  config = let
    themeDir = "task";
    themeFile = "${themeName}.theme";
    themeName = "base16";
  in lib.mkIf config.stylix.targets.taskwarrior.enable {
    programs.taskwarrior.colorTheme = "${config.xdg.configHome}/${themeDir}/${themeName}";

    xdg.configFile."${themeDir}/${themeFile}".source = with config.lib.stylix.colors; let
      base00 = "rgb${scale base00-rgb-r}${scale base00-rgb-g}${scale base00-rgb-b}";
      base01 = "rgb${scale base01-rgb-r}${scale base01-rgb-g}${scale base01-rgb-b}";
      base02 = "rgb${scale base02-rgb-r}${scale base02-rgb-g}${scale base02-rgb-b}";
      base03 = "rgb${scale base03-rgb-r}${scale base03-rgb-g}${scale base03-rgb-b}";
      base04 = "rgb${scale base04-rgb-r}${scale base04-rgb-g}${scale base04-rgb-b}";
      base05 = "rgb${scale base05-rgb-r}${scale base05-rgb-g}${scale base05-rgb-b}";
      base06 = "rgb${scale base06-rgb-r}${scale base06-rgb-g}${scale base06-rgb-b}";
      base07 = "rgb${scale base07-rgb-r}${scale base07-rgb-g}${scale base07-rgb-b}";
      base08 = "rgb${scale base08-rgb-r}${scale base08-rgb-g}${scale base08-rgb-b}";
      base09 = "rgb${scale base09-rgb-r}${scale base09-rgb-g}${scale base09-rgb-b}";
      base0A = "rgb${scale base0A-rgb-r}${scale base0A-rgb-g}${scale base0A-rgb-b}";
      base0B = "rgb${scale base0B-rgb-r}${scale base0B-rgb-g}${scale base0B-rgb-b}";
      base0C = "rgb${scale base0C-rgb-r}${scale base0C-rgb-g}${scale base0C-rgb-b}";
      base0D = "rgb${scale base0D-rgb-r}${scale base0D-rgb-g}${scale base0D-rgb-b}";
      base0E = "rgb${scale base0E-rgb-r}${scale base0E-rgb-g}${scale base0E-rgb-b}";
      base0F = "rgb${scale base0F-rgb-r}${scale base0F-rgb-g}${scale base0F-rgb-b}";

      # Map the traditional RGB range (0--255) to Taskwarrior's RGB range
      # (0--5).
      scale = let
        factor = 6.0 / 255.0;
      in rgb: toString (builtins.floor (lib.toInt rgb * factor + 0.5));
    in config.lib.stylix.colors {
      extension = "theme";
      template = ./template.theme.mustache;
    };
  };
}
