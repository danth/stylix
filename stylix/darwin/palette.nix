args:
{ config, lib, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config = lib.mkIf config.stylix.enable {
    environment.etc = config.stylix.generated.fileTree;
  };
}
