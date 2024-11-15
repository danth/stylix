{
  lib,
  pkgs,
  config,
  ...
}:

let
  theme = pkgs.callPackage ./theme.nix {
    inherit (config.lib.stylix) colors templates;
  };

in
{
  options.stylix.targets.gnome.enable =
    config.lib.stylix.mkEnableTarget "GNOME and GDM" true;

  config =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.gnome.enable
        && (
          config.services.xserver.desktopManager.gnome.enable
          || config.services.xserver.displayManager.gdm.enable
        )
      )
      {
        # As Stylix is controlling the wallpaper, there is no need for this
        # pack of default wallpapers to be installed.
        # If you want to use one, you can set stylix.image to something like
        # "${pkgs.gnome-backgrounds}/path/to/your/preferred/background"
        # which will then download the pack regardless of its exclusion below.
        environment.gnome.excludePackages = [ pkgs.gnome-backgrounds ];

        nixpkgs.overlays = [
          (_: super: {
            gnome-shell = super.gnome-shell.overrideAttrs (oldAttrs: {
              # Themes are usually applied via an extension, but extensions are
              # not available on the login screen. The only way to change the
              # theme there is by replacing the default.
              postFixup =
                (oldAttrs.postFixup or "")
                + ''
                  cp ${theme}/share/gnome-shell/gnome-shell-theme.gresource \
                    $out/share/gnome-shell/gnome-shell-theme.gresource
                '';
              patches = (oldAttrs.patches or [ ]) ++ [
                ./shell_remove_dark_mode.patch
              ];
            });
          })
        ];

        # Cursor settings are usually applied via Home Manager,
        # but the login screen uses a separate database.
        environment.systemPackages = [ config.stylix.cursor.package ];
        programs.dconf.profiles.gdm.databases = [
          {
            lockAll = true;
            settings."org/gnome/desktop/interface" = {
              cursor-theme = config.stylix.cursor.name;
              cursor-size = lib.gvariant.mkInt32 config.stylix.cursor.size;
            };
          }
        ];
      };
}
