{
  config,
  lib,
  pkgs,
  ...
}:

{
  config =
    lib.mkIf (config.stylix.testbed.ui.graphicalEnvironment or null == "gnome")
      {
        services.xserver = {
          enable = true;
          displayManager.gdm.enable = true;
          desktopManager.gnome.enable = true;
        };

        # Disable the GNOME tutorial which pops up on first login.
        environment.gnome.excludePackages = [ pkgs.gnome-tour ];
      };
}
