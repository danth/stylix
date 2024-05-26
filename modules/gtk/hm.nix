{ pkgs, config, lib, ... }:

with lib;

let
  cfg = config.stylix.targets.gtk;

  baseCss = config.lib.stylix.colors {
    template = ./gtk.mustache;
    extension = "css";
  };

  finalCss = pkgs.runCommandLocal "gtk.css" {} ''
    cat ${baseCss} >>$out
    echo ${escapeShellArg cfg.extraCss} >>$out
  '';

in {
  options.stylix.targets.gtk = {
    enable = config.lib.stylix.mkEnableTarget
      "all GTK3, GTK4 and Libadwaita apps" true;

    extraCss = mkOption {
      description = ''
        Extra code added to `gtk-3.0/gtk.css` and `gtk-4.0/gtk.css`.
      '';
      type = types.lines;
      default = "";
      example = ''
        // Remove rounded corners
        window.background { border-radius: 0; }
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # programs.dconf.enable = true; required in system config
    gtk = {
      enable = true;
      font = let
          inherit (config.stylix) fonts;
          sansSerif = builtins.head fonts.sansSerif;
        in {
          inherit (sansSerif) package name;
          size = fonts.sizes.applications;
        };
      theme = {
        package = pkgs.adw-gtk3;
        name = "adw-gtk3";
      };
    };

    xdg.configFile = {
      "gtk-3.0/gtk.css".source = finalCss;
      "gtk-4.0/gtk.css".source = finalCss;
    };
  };
}
