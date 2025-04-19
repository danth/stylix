{ lib, pkgs, ... }:

let
  package = pkgs.yazi;
in

{
  stylix.testbed.application = {
    enable = true;
    name = "yazi";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.yazi = {
      enable = true;
      inherit package;
    };

    home.packages = [
      pkgs.nerd-fonts.fira-mono
    ];
  };
}
