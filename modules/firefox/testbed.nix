{ pkgs, ... }:

let
  package = pkgs.firefox;

in
{
  stylix.testbed.application = {
    enable = true;
    name = "firefox";
    inherit package;
  };

  home-manager.sharedModules = [
    {
      programs.firefox = {
        enable = true;
        inherit package;
        profiles.stylix.isDefault = true;
      };

      stylix.targets.firefox.profileNames = [ "stylix" ];
    }
  ];
}
