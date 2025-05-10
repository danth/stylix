{ lib, pkgs, ... }:

let
  package = pkgs.rstudio;
in
{
  stylix.testbed.ui.command.text = lib.getExe package;

  home-manager.sharedModules = lib.singleton {
    home.packages = [ package ];
  };
}
