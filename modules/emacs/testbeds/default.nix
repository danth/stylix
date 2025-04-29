{ lib, pkgs, ... }:

let
  package = pkgs.emacs;
in
{
  stylix.testbed.ui.application = {
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
