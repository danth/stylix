{ pkgs, config, lib, ... }:

{
  options.stylix.targets.gtk.enable =
    config.lib.stylix.mkEnableTarget "all GTK3, GTK4 and Libadwaita apps" true;

  config = lib.mkIf config.stylix.targets.gtk.enable {
    # Required for Home Manager's GTK settings to work
    programs.dconf.enable = true;
  };
}
