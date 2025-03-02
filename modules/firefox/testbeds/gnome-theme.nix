{ lib, pkgs, ... }:

let
  package = pkgs.firefox;
  profileName = "stylix";
in
{
  stylix.testbed.application = {
    enable = true;
    name = "firefox";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.firefox = {
      enable = true;
      inherit package;
      profiles.${profileName}.isDefault = true;
    };

    stylix.targets.firefox = {
      profileNames = [ profileName ];
      firefoxGnomeTheme.enable = true;
    };
  };
}
