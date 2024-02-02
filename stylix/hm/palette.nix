args:
{ config, lib, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config = lib.mkIf config.stylix.enable {
    xdg.configFile = config.stylix.generated.fileTree;
  };
}
