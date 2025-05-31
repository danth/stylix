{ lib, pkgs, ... }:

let
  package = pkgs.rstudio;
in
{
  stylix.testbed.ui.command.text = "rstudio";

  home-manager.sharedModules = lib.singleton {
    home.packages = [
      package
    ];
  };
}
