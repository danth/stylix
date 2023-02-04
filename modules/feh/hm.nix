{ pkgs, config, lib, ... }:

{
  options.stylix.targets.feh.enable =
    config.lib.stylix.mkEnableTarget
    "the desktop background using Feh"
    true;

  config.xsession.initExtra =
    lib.mkIf config.stylix.targets.feh.enable
    "${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${config.stylix.image}";
}
