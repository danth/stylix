{
  pkgs,
  config,
  lib,
  ...
}:

let
  inherit (config.stylix.fonts) sansSerif serif monospace;
  fontSize = toString config.stylix.fonts.sizes.applications;
  documentFontSize = toString (config.stylix.fonts.sizes.applications - 1);

  activator = pkgs.writeShellApplication {
    name = "stylix-activate-gnome";
    text = ''
      get_exe() {
        for directory in /run/current-system/sw/bin /usr/bin /bin; do
          if [[ -f "$directory/$1" ]]; then
            printf '%s\n' "$directory/$1"
            return 0
          fi
        done
        echo "Skipping '$1': command not found"
        return 1
      }

      if gnome_extensions="$(get_exe gnome-extensions)"; then
        extension='user-theme@gnome-shell-extensions.gcampax.github.com'

        case "$1" in
          reload)
            "$gnome_extensions" disable "$extension"
            "$gnome_extensions" enable "$extension"
            ;;
          enable)
            "$gnome_extensions" enable "$extension"
            ;;
        esac
      fi
    '';
  };

in
{
  options.stylix.targets.gnome.enable =
    config.lib.stylix.mkEnableTarget "GNOME" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.gnome.enable) {
    dconf.settings = {
      "org/gnome/desktop/background" = {
        color-shading-type = "solid";
        picture-options =
          let
            inherit (config.stylix) imageScalingMode;
          in
          if imageScalingMode == "fit" then
            "scaled"
          else if imageScalingMode == "fill" then
            "zoom"
          else if imageScalingMode == "stretch" then
            "stretched"
          else if imageScalingMode == "center" then
            "centered"
          # Seemingly no tile support... :(
          else
            "zoom";
        picture-uri = "file://${config.stylix.image}";
        picture-uri-dark = "file://${config.stylix.image}";
      };

      "org/gnome/desktop/interface" = {
        # We show the same colours regardless of this setting, and the quick
        # settings tile is removed. The value is still used by Epiphany to
        # request dark mode for websites which support it.
        color-scheme =
          if config.stylix.polarity == "dark" then "prefer-dark" else "default";

        # Some GTK apps will use these font settings if they exist.
        # i.e emacs-pgtk.
        font-name = "${sansSerif.name} ${fontSize}";
        document-font-name = "${serif.name}  ${documentFontSize}";
        monospace-font-name = "${monospace.name} ${fontSize}";
      };

      "org/gnome/shell/extensions/user-theme".name = "Stylix";
    };

    xdg = {
      dataFile."themes/Stylix/gnome-shell/gnome-shell.css" = {
        source =
          let
            theme = pkgs.callPackage ./theme.nix {
              inherit (config.lib.stylix) colors templates;
            };
          in
          "${theme}/share/gnome-shell/gnome-shell.css";

        # Reload the extension so the new theme is applied immediately.
        # (The extension doesn't watch the file for changes.)
        onChange = "${lib.getExe activator} reload";
      };

      # Enable the extension after logging in.
      configFile."autostart/stylix-activate-gnome.desktop".text = ''
        [Desktop Entry]
        Type=Application
        Exec=${lib.getExe activator} enable
        Name=Stylix: enable User Themes extension for GNOME Shell
      '';
    };
  };
}
