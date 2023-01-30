{ pkgs, config, lib, ... }:

let
  css = config.lib.stylix.colors {
    template = builtins.readFile ./gtk.mustache;
    extension = "css";
  };
 
in {
  options.stylix.targets.gtk.enable =
    config.lib.stylix.mkEnableTarget "all GTK3, GTK4 and Libadwaita apps" true;

  config = lib.mkIf config.stylix.targets.gtk.enable {
    # programs.dconf.enable = true; required in system config
    gtk = {
      enable = true;
      font = config.stylix.fonts.sansSerif;
      theme = {
        package = pkgs.adw-gtk3;
        name = "adw-gtk3";
      };
    };

    xdg.configFile = {
      "gtk-3.0/gtk.css".source = css;
      "gtk-4.0/gtk.css".source = css;
    };
  };
}
