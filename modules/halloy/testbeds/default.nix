{ pkgs, lib, ... }:

let
  package = pkgs.halloy;
in
{
  stylix.testbed.ui.application = {
    name = "org.squidowl.halloy";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.halloy = {
      enable = true;
      inherit package;
      settings = {
        servers.liberachat = {
          nickname = "stylix-testbed";
          server = "irc.libera.chat";
          channels = [ "#halloy" ];
        };
      };
    };
  };
}
