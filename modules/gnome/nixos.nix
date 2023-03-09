{ pkgs, config, ... }:

let 
  # We use this imported lib instead of the one from the module arguments
  # to avoid infinite loops if the lib in arguments depends on nixpkgs.overlays
  lib = (builtins.getFlake "github:nix-community/nixpkgs.lib/c9d4f2476046c6a7a2ce3c2118c48455bf0272ea").lib;
in {
  options.stylix.targets.gnome.enable =
    lib.mkOption {
      description = "Whether to style GNOME";
      type = lib.types.bool;
      default = config.stylix.autoEnable 
             && config.services.xserver.desktopManager.gnome.enable;
    };

  config = lib.mkIf config.stylix.targets.gnome.enable {
    # As Stylix is controlling the wallpaper, there is no need for this
    # pack of default wallpapers to be installed.
    # If you want to use one, you can set stylix.image to something like
    # "${pkgs.gnome.gnome-backgrounds}/path/to/your/preferred/background"
    # which will then download the pack regardless of its exclusion below.
    environment.gnome.excludePackages = [ pkgs.gnome.gnome-backgrounds ];

    nixpkgs.overlays = [(self: super: {
      gnome = super.gnome.overrideScope' (gnomeSelf: gnomeSuper: {
        gnome-shell = gnomeSuper.gnome-shell.overrideAttrs (oldAttrs: {
          postFixup =
            let theme = import ./theme.nix { inherit config; pkgs = super; };
            in ''
              cp ${theme}/share/gnome-shell/gnome-shell-theme.gresource \
                $out/share/gnome-shell/gnome-shell-theme.gresource
            '';
          patches = (oldAttrs.patches or []) ++ [
            ./shell_remove_dark_mode.patch
          ];
        });
      });
    })];
  };
}
