{ config, lib, ... }:

let
  colors = config.lib.stylix.colors;
in {
  options.stylix.targets.zathura.enable =
    config.lib.stylix.mkEnableTarget "Zathura" true;

  config = lib.mkIf config.stylix.targets.zathura.enable {
    # Taken from here:
    #   https://github.com/doenerkebap/base16-zathura
    programs.zathura.options = {
      default-bg = "#${colors.base00}";
      default-fg = "#${colors.base01}";
      statusbar-fg = "#${colors.base04}";
      statusbar-bg = "#${colors.base02}";
      inputbar-bg = "#${colors.base00}";
      inputbar-fg = "#${colors.base07}";
      notification-bg = "#${colors.base00}";
      notification-fg = "#${colors.base07}";
      notification-error-bg = "#${colors.base00}";
      notification-error-fg = "#${colors.base08}";
      notification-warning-bg = "#${colors.base00}";
      notification-warning-fg = "#${colors.base08}";
      highlight-color = "#${colors.base0A}";
      highlight-active-color = "#${colors.base0D}";
      completion-bg = "#${colors.base01}";
      completion-fg = "#${colors.base0D}";
      completion-highlight-fg = "#${colors.base07}";
      completion-highlight-bg = "#${colors.base0D}";
      recolor-lightcolor = "#${colors.base00}";
      recolor-darkcolor = "#${colors.base06}";
      recolor = false;
      recolor-keephue = false;
    };
  };
}
