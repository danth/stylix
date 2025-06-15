{
  config,
  lib,
  ...
}:

{
  config =
    lib.mkIf (config.stylix.testbed.ui.graphicalEnvironment or null == "kde")
      {
        services = {
          displayManager.sddm.enable = true;

          xserver = {
            enable = true;
            desktopManager.plasma6.enable = true;
          };
        };
      };
}
