{ pkgs, config, lib, ... }:

with lib;

with config.services.xserver.windowManager;
let enable = xmonad.enable || i3.enable;

in {
  services.xserver.displayManager.sessionCommands = mkIf enable
    "${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${config.stylix.image}";
}
