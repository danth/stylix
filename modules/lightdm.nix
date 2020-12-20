{ config, ... }:

{
  services.xserver.displayManager.lightdm.background = config.stylix.image;
}
