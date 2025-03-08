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
  cursorCfg = config.stylix.cursor;
  iconCfg = config.stylix.iconTheme;
  fontCfg = config.stylix.fonts;
  inherit (config.stylix) polarity;

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

        # Cursor and icon settings are usually applied via Home Manager,
        # but the login screen uses a separate database.
        services.displayManager.environment.XDG_DATA_DIRS =
          (lib.makeSearchPath "share" (
            [
              iconCfg.package
            ]
            ++ fontCfg.packages
          ))
          + ":";
        environment.systemPackages = [
          cursorCfg.package
        ] ++ fontCfg.packages;
        programs.dconf.profiles.gdm.databases = [
          {
            lockAll = true;
            settings."org/gnome/desktop/interface" = {
              cursor-theme = cursorCfg.name;
              cursor-size = lib.gvariant.mkInt32 cursorCfg.size;

              icon-theme = builtins.head (
                lib.filter (x: null != x) [
                  (
                    {
                      inherit (iconCfg) dark light;
                    }
                    ."${polarity}" or null
                  )
                  iconCfg.dark
                  iconCfg.light
                ]
              );

              font-name = fontCfg.sansSerif.name;
            };
          }
        ];
      };
}
