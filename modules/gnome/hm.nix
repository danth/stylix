{ pkgs, config, lib, ... }@args:

with lib;

{
  options.stylix.targets.gnome.enable =
    config.lib.stylix.mkEnableTarget "GNOME" true;

  config = mkIf config.stylix.targets.gnome.enable {
    dconf.settings = {
      "org/gnome/desktop/background" = {
        color-shading-type = "solid";
        picture-options = "zoom";
        picture-uri = "file://${config.stylix.image}";
        picture-uri-dark = "file://${config.stylix.image}";
      };

      "org/gnome/desktop/interface" = with config.stylix.fonts ; {
        # We show the same colours regardless of this setting, and the quick
        # settings tile is removed. The value is still used by Epiphany to
        # request dark mode for websites which support it.
        color-scheme =
          if config.stylix.polarity == "dark"
          then "prefer-dark"
          else "default";

        # Some GTK apps will use these font settings if they exist.
        # i.e emacs-pgtk.
        font-name = "${sansSerif.name} ${toString sizes.applications}";
        document-font-name = "${serif.name}  ${toString (sizes.applications - 1)}";
        monospace-font-name = "${monospace.name} ${toString sizes.applications}";
      };

      "org/gnome/shell/extensions/user-theme".name = "Stylix";
    };

    xdg.dataFile."themes/Stylix/gnome-shell/gnome-shell.css" = {
      source =
        let theme = import ./theme.nix args;
        in "${theme}/share/gnome-shell/gnome-shell.css";
      onChange = ''
        if [[ -x "$(command -v gnome-extensions)" ]]; then
          gnome-extensions disable user-theme@gnome-shell-extensions.gcampax.github.com
          gnome-extensions enable user-theme@gnome-shell-extensions.gcampax.github.com
        fi
      '';
    };
  };
}
