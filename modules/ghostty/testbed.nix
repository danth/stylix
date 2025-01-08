{ lib, pkgs, ... }:
let
  package = pkgs.ghostty;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "com.mitchellh.ghostty";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.ghostty = {
      enable = true;
      inherit package;
    };
  };
}
