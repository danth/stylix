{ lib, pkgs, ... }:

let
  package = pkgs.kitty;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "kitty";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.kitty = {
      enable = true;
      inherit package;
    };
  };
}
