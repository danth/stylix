args:
{ config, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config = {
    environment.etc = config.stylix.generated.fileTree;
  };
}
