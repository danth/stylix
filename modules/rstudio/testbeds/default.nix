{ lib, pkgs, ... }:

let
  package = pkgs.rstudio;
in
{
  stylix.testbed.ui.application = {
    name = "RStudio";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.rstudio = {
      enable = true;
      inherit package;
    };
  };
}
