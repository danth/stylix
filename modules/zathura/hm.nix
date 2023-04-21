{ config, lib, ... }:

with config.lib.stylix.colors;
let
  zathOpacity = lib.toHexString ((((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100));
in {
  options.stylix.targets.zathura.enable =
    config.lib.stylix.mkEnableTarget "Zathura" true;

  config = lib.mkIf config.stylix.targets.zathura.enable {
    # Taken from here:
    #   https://github.com/doenerkebap/base16-zathura
    programs.zathura.options = {
      default-bg = "#rgba(${base00-rgb-r}, ${base00-rgb-g}, ${base00-rgb-b}, ${zathOpacity})";
      default-fg = "#${base01}";
      statusbar-fg = "#${base04}";
      statusbar-bg = "#${base02}";
      inputbar-bg = "#${base00}";
      inputbar-fg = "#${base07}";
      notification-bg = "#${base00}";
      notification-fg = "#${base07}";
      notification-error-bg = "#${base00}";
      notification-error-fg = "#${base08}";
      notification-warning-bg = "#${base00}";
      notification-warning-fg = "#${base08}";
      highlight-color = "#${base0A}";
      highlight-active-color = "#${base0D}";
      completion-bg = "#${base01}";
      completion-fg = "#${base0D}";
      completion-highlight-fg = "#${base07}";
      completion-highlight-bg = "#${base0D}";
      recolor-lightcolor = "#${base00}";
      recolor-darkcolor = "#${base06}";
      recolor = false;
      recolor-keephue = false;
    };
  };
}
