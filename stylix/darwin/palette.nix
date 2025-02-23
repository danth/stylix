{ config, lib, ... }:

{
  config = lib.mkIf config.stylix.enable {
    environment.etc = config.stylix.generated.fileTree;
  };
}
