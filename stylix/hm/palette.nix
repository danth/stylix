args:
{ config, ... }:

{
  imports = [ (import ../palette.nix args) ];

  config = {
    xdg.configFile = config.stylix.generated.fileTree;
  };
}
