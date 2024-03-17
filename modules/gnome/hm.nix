{ pkgs, config, lib, ... }@args:

with lib;

let
  wallpaper = config.stylix.wallpaper.unpack {
    slideshow =
      { files, delay, ... }:
      let
        slide = file: ''
          <static>
            <duration>${toString delay}</duration>
            <file>${file}</file>
          </static>
        '';
        slideshow = pkgs.writeText "slideshow.xml" ''
          <?xml version="1.0"?>
          <background>
          ${concatMapStrings slide files}
          </background>
        '';
      in "file://${slideshow}";

    image = { file, ... }: "file://${file}";
  };

in {
  options.stylix.targets.gnome.enable =
    config.lib.stylix.mkEnableTarget "GNOME" true;

  config = mkIf config.stylix.targets.gnome.enable {
    dconf.settings = {
      "org/gnome/desktop/background" = {
        picture-uri = wallpaper;
        picture-uri-dark = wallpaper;
        picture-options = "zoom";
        color-shading-type = "solid";
        primary-color = config.stylix.colors.withHashtag.base00;
        secondary-color = config.stylix.colors.withHashtag.base01;
      };
      
      "org/gnome/desktop/interface" = with config.stylix.fonts ; {
        # Some GTK apps will use these font settings if they exist.
        # i.e emacs-pgtk.
        font-name = "${sansSerif.name} ${toString sizes.applications}";
        document-font-name = "${serif.name}  ${toString (sizes.applications - 1)}";
        monospace-font-name = "${monospace.name} ${toString sizes.applications}";

        color-scheme =
          if (config.lib.stylix.polarityFrom config.stylix.colors) == "dark"
          then "prefer-dark"
          else "default";
      };
      
      "org/gnome/shell/extensions/user-theme".name = "Stylix";
    };

    xdg.dataFile."themes/Stylix/gnome-shell/gnome-shell.css" = {
      source =
        let theme = import ./theme.nix args;
        in "${theme}/share/gnome-shell/gnome-shell.css";
      onChange = ''
        if [ -x "$(command -v gnome-extensions)" ]; then
          gnome-extensions disable user-theme@gnome-shell-extensions.gcampax.github.com
          gnome-extensions enable user-theme@gnome-shell-extensions.gcampax.github.com
        fi
      '';
    };
  };
}
