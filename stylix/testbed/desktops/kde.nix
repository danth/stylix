{
  config,
  lib,
  ...
}:

{
  config = lib.mkIf (config.stylix.testbed.ui.desktop or null == "kde") {
    services = {
      displayManager.sddm.enable = true;

      xserver = {
        enable = true;
        desktopManager.plasma5.enable = true;
      };
    };
  };
}
