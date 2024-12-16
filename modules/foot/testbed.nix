{ pkgs, ... }:

let package = pkgs.foot;

in {
  stylix.testbed.application = {
    enable = true;
    name = "org.codeberg.dnkl.foot";
    inherit package;
  };

  home-manager.sharedModules = [{
    programs.foot = {
      enable = true;
      inherit package;
    };
  }];
}