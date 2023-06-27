{ pkgs, config, lib, ... }:

{
  options.stylix.targets.zellij.enable =
    config.lib.stylix.mkEnableTarget "zellij" config.programs.zellij.enable ;

  config = lib.mkIf config.stylix.targets.zellij.enable {
    programs.zellij.settings = {
        theme = "stylix";
        themes.stylix = with config.lib.stylix.colors.withHashtag; {
            bg = base03;
            fg = base05;
            red = base08;
            green = base0B;
            blue = base0D;
            yellow = base0A;
            magenta = base0E;
            orange = base09;
            cyan = base0C;
            black = base03;
            white = base05;
        };
    };
  };

}
