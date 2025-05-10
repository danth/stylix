{ lib, pkgs, ... }:

let
  package = pkgs.wofi;
in
{
  stylix.testbed.ui.command.text =
    "${lib.getExe package} --allow-images --show drun";

  home-manager.sharedModules = lib.singleton {
    programs.wofi = {
      enable = true;
      inherit package;
    };
  };
}
