{ pkgs, ... }:

let
  package = pkgs.qutebrowser;

in
{
  stylix.testbed.application = {
    enable = true;
    name = "org.qutebrowser.qutebrowser";
    inherit package;
  };

  home-manager.sharedModules = [
    {
      programs.qutebrowser = {
        enable = true;
        inherit package;
      };
    }
  ];
}
