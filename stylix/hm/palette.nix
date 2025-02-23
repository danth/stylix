{ config, lib, ... }:

{
  config = lib.mkIf config.stylix.enable {
    xdg.configFile = config.stylix.generated.fileTree;
  };
}
