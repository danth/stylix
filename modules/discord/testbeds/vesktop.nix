{ pkgs, lib, ... }:

let
  package = pkgs.vesktop;
in
{
  stylix.testbed.ui.application = {
    name = "vesktop";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.vesktop = {
      enable = true;
      inherit package;
    };
  };
}
