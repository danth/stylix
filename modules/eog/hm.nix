{ config, lib, ... }:

{
  options.stylix.targets.eog.enable =
    config.lib.stylix.mkEnableTarget "Eye of GNOME Image Viewer" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.eog.enable) {
    dconf.settings."org/gnome/eog/view" = {
      # transparency = "background"; # Disables the grey and white check pattern.
      background-color = "#${config.lib.stylix.colors.base00}";
    };
  };
}
