{ config, lib, ... }:

{
  config.fonts = lib.mkIf config.stylix.enable {
    inherit (config.stylix.fonts) packages;
  };
}
