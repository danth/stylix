{ lib, pkgs, ... }:

let
  package = pkgs.foliate;
in
{
  stylix.testbed.ui.application = {
    name = "com.github.johnfactotum.Foliate";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.foliate = {
      enable = true;
      inherit package;
    };
  };
}
