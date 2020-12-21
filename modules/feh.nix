{ pkgs, config, ... }:

{
  # Set background image
  services.xserver.displayManager.sessionCommands =
    "${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${config.stylix.image}";
}
