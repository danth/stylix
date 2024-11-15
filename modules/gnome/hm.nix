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

    xdg.dataFile."themes/Stylix/gnome-shell/gnome-shell.css" = {
      source =
        let
          theme = pkgs.callPackage ./theme.nix {
            inherit (config.lib.stylix) colors templates;
          };
        in
        "${theme}/share/gnome-shell/gnome-shell.css";
      onChange = ''
        if [[ -x "$(command -v gnome-extensions)" ]]; then
          gnome-extensions disable user-theme@gnome-shell-extensions.gcampax.github.com
          gnome-extensions enable user-theme@gnome-shell-extensions.gcampax.github.com
        fi
      '';
    };
  };
}
