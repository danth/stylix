{ pkgs, config, lib, ... }:

{
  options.stylix.targets.feh.enable =
    config.lib.stylix.mkEnableTarget
    "the desktop background using Feh"
    (with config.services.xserver.windowManager; xmonad.enable || i3.enable);

  config.services.xserver.displayManager.sessionCommands =
    lib.mkIf config.stylix.targets.feh.enable
    "${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${config.stylix.image}";
}
