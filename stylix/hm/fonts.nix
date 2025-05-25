{ config, lib, ... }:
{
  config = lib.mkIf config.stylix.enable {
    fonts.fontconfig.enable = true;
  };
}
