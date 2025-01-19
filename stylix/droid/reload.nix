{ config, lib, ... }:

{
  config = lib.mkIf config.stylix.enable {
    android-integration.termux-reload-settings.enable = true;
    build.activationAfter.reloadTermuxSettings = ''
      $DRY_RUN_CMD termux-reload-settings
    '';
  };
}
