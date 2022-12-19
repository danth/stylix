{ pkgs, config, lib, ... }:

with lib;

let
  colors = config.lib.stylix.colors {
    template = builtins.readFile ./colors.mustache;
    extension = "scss";
  };

  theme = pkgs.stdenv.mkDerivation {
    name = "stylix-gnome-shell.css";

    src = pkgs.fetchurl {
      url = "mirror://gnome/sources/gnome-shell/43/gnome-shell-43.2.tar.xz";
      sha256 = "52/UvpNCQQ7p+9zday2Bxv8GDnyMxaDxyuanq6JdGGA=";
    };
    sourceRoot = "gnome-shell-43.2/data/theme";

    patches = [ ./shell.patch ];
    postPatch = ''
      rm gnome-shell-sass/{_colors.scss,_palette.scss}
      cp ${colors} gnome-shell-sass/_colors.scss
    '';

    nativeBuildInputs = with pkgs; [ sass ];
    buildPhase = ''
      sass gnome-shell.scss >$out
    '';

    installPhase = "true";
  };

in {
  options.stylix.targets.gnome.enable =
    config.lib.stylix.mkEnableTarget "GNOME" config.services.xserver.desktopManager.gnome.enable;

  config = mkIf config.stylix.targets.gnome.enable {
    home-manager.sharedModules = [({ lib, ... }: {
      dconf.settings = {
        "org/gnome/desktop/background" = {
          color-shading-type = "solid";
          picture-options = "zoom";
          picture-uri = "file://${config.stylix.image}";
          picture-uri-dark = "file://${config.stylix.image}";
        };

        "org/gnome/desktop/interface".color-scheme =
          if config.stylix.polarity == "dark"
          then "prefer-dark"
          else "default";

        "org/gnome/shell/extensions/user-theme".name = "Stylix";
      };

      xdg.dataFile."themes/Stylix/gnome-shell/gnome-shell.css" = {
        source = theme;
        onChange = ''
          gnome-extensions disable user-theme@gnome-shell-extensions.gcampax.github.com
          gnome-extensions enable user-theme@gnome-shell-extensions.gcampax.github.com
        '';
      };
    })];

    # As Stylix is controlling the wallpaper, there is no need for this
    # pack of default wallpapers to be installed.
    # If you want to use one, you can set stylix.image to something like
    # "${pkgs.gnome.gnome-backgrounds}/path/to/your/preferred/background"
    # which will then download the pack regardless of its exclusion below.
    environment.gnome.excludePackages = [ pkgs.gnome.gnome-backgrounds ];
  };
}
