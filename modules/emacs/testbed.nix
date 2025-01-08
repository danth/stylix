{ lib, pkgs, ... }:

let
  package = pkgs.emacs;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "emacs";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.emacs = {
      enable = true;
      inherit package;
    };
  };
}
