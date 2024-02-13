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
      base00 = taskwarriorRgb base00-rgb-r base00-rgb-g base00-rgb-b;
      base01 = taskwarriorRgb base01-rgb-r base01-rgb-g base01-rgb-b;
      base02 = taskwarriorRgb base02-rgb-r base02-rgb-g base02-rgb-b;
      base03 = taskwarriorRgb base03-rgb-r base03-rgb-g base03-rgb-b;
      base04 = taskwarriorRgb base04-rgb-r base04-rgb-g base04-rgb-b;
      base05 = taskwarriorRgb base05-rgb-r base05-rgb-g base05-rgb-b;
      base06 = taskwarriorRgb base06-rgb-r base06-rgb-g base06-rgb-b;
      base07 = taskwarriorRgb base07-rgb-r base07-rgb-g base07-rgb-b;
      base08 = taskwarriorRgb base08-rgb-r base08-rgb-g base08-rgb-b;
      base09 = taskwarriorRgb base09-rgb-r base09-rgb-g base09-rgb-b;
      base0A = taskwarriorRgb base0A-rgb-r base0A-rgb-g base0A-rgb-b;
      base0B = taskwarriorRgb base0B-rgb-r base0B-rgb-g base0B-rgb-b;
      base0C = taskwarriorRgb base0C-rgb-r base0C-rgb-g base0C-rgb-b;
      base0D = taskwarriorRgb base0D-rgb-r base0D-rgb-g base0D-rgb-b;
      base0E = taskwarriorRgb base0E-rgb-r base0E-rgb-g base0E-rgb-b;
      base0F = taskwarriorRgb base0F-rgb-r base0F-rgb-g base0F-rgb-b;

      # Map the traditional RGB range (0--255) to Taskwarrior's RGB range
      # (0--5).
      taskwarriorRgb = let
        convert = component: toString (scale component);
        factor = 6.0 / 255.0;
        scale = component: builtins.floor (lib.toInt component * factor + 0.5);
      in red: green: blue: let
        taskwarrior = {
          blue = convert blue;
          green = convert green;
          red = convert red;
        };
      in "rgb${taskwarrior.red}${taskwarrior.green}${taskwarrior.blue}";
    in config.lib.stylix.colors {
      extension = "theme";
      template = ./template.theme.mustache;
    };
  };
}
