{ pkgs, config, lib, ... }:

with lib;

{
  options.stylix.targets.gnome.enable =
    config.lib.stylix.mkEnableTarget "GNOME" config.services.xserver.desktopManager.gnome.enable;

  config = mkIf config.stylix.targets.gnome.enable {
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
            let theme = import ./theme.nix { inherit pkgs config; };
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
