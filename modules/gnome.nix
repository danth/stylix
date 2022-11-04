{ pkgs, config, lib, ... }:

{
  options.stylix.targets.gnome.enable =
    config.lib.stylix.mkEnableTarget "GNOME" true;

  config.home-manager.sharedModules =
    if config.stylix.polarity == "dark"
    then
      lib.mkIf config.stylix.targets.gnome.enable [{
        dconf.settings."org/gnome/desktop/background" = {
          color-shading-type = "solid";
          picture-options = "zoom";
          picture-uri-dark = "file://${config.stylix.image}";
        };
        dconf.settings."org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
        };
      }]
    else
      lib.mkIf config.stylix.targets.gnome.enable [{
        dconf.settings."org/gnome/desktop/background" = {
          color-shading-type = "solid";
          picture-options = "zoom";
          picture-uri = "file://${config.stylix.image}";
        };
      }];
}
