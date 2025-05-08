args:
{ config, lib, ... }:

{
  imports = [ (lib.modules.importApply ../palette.nix args) ];

  config = lib.mkIf config.stylix.enable {
    environment.etc = config.stylix.generated.fileTree;
  };
}
