{ pkgs, config, lib, ... }:

let
  colors = config.lib.stylix.colors; 
in
{

  options.stylix.targets.zellij.enable =
    config.lib.stylix.mkEnableTarget "zellij" config.programs.zellij.enable ;

  config = lib.mkIf config.stylix.targets.zellij.enable {
    programs.zellij.settings = {
        theme = "stylix";
        themes.stylix = {
            bg = "#${colors.base04}";
            fg = "#${colors.base05}";
            red = "#${colors.base08}";
            green = "#${colors.base0A}";
            blue = "#${colors.base0D}";
            yellow = "#${colors.base0B}";
            magenta = "#${colors.base0E}";
            orange = "#${colors.base09}";
            cyan = "#${colors.base07}";
            black = "#${colors.base03}";
            white = "#${colors.base05}";
        };
    };
  };

}
