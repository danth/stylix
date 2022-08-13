{ pkgs, config, lib, ... }:

{
  options.stylix.targets.gnome.enable =
    config.lib.stylix.mkEnableTarget "GNOME" true;

  config.home-manager.sharedModules =
    lib.mkIf config.stylix.targets.gnome.enable [{
      dconf.settings."org/gnome/desktop/background" = {
        color-shading-type = "solid";
        picture-options = "zoom";
        picture-uri = "file://${config.stylix.image}";
      };
    }];
}
