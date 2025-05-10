{ lib, pkgs, ... }:

let
  package = pkgs.rstudioWrapper;
in
{
  stylix.testbed.ui.command.text = "rstudio";

  home-manager.sharedModules = lib.singleton {
    home.packages = [ package ];
  };
}
