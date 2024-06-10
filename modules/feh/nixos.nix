{ pkgs, config, lib, ... }:

{
  options.stylix.targets.feh.enable =
    config.lib.stylix.mkEnableTarget
    "the desktop background using Feh"
    true;

  config.services.xserver.displayManager.sessionCommands =
    lib.mkIf (
      config.stylix.enable
      && config.stylix.targets.feh.enable
      && (
        with config.services.xserver.windowManager;
        xmonad.enable
        || i3.enable
      )
    )
    "${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${config.stylix.image}";
}
