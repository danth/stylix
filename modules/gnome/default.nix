{ pkgs, config, lib, ... }:

with lib;

{
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
      };
    })];

    # As Stylix is controlling the wallpaper, there is no need for this
    # pack of default wallpapers to be installed.
    # If you want to use one, you can set stylix.image to something like
    # "${pkgs.gnome.gnome-backgrounds}/path/to/your/preferred/background"
    # which will then download the pack regardless of its exclusion below.
    environment.gnome.excludePackages = [ pkgs.gnome.gnome-backgrounds ];

    nixpkgs.overlays = [(self: super: {
      gnome = super.gnome.overrideScope' (gnomeSelf: gnomeSuper: {
        gnome-shell = gnomeSuper.gnome-shell.overrideAttrs (oldAttrs: {
          postPatch = let
            colors = config.lib.stylix.colors {
              template = builtins.readFile ./colors.mustache;
              extension = "scss";
            };
          in (oldAttrs.postPatch or "") + ''
            rm data/theme/gnome-shell-sass/{_colors.scss,_palette.scss}
            cp ${colors} data/theme/gnome-shell-sass/_colors.scss
          '';
          patches = (oldAttrs.patches or []) ++ [ ./shell.patch ];
        });
      });
    })];
  };
}
