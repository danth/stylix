{ pkgs, ... }:

let
  package = pkgs.emacs;

in
{
  stylix.testbed.application = {
    enable = true;
    name = "emacs";
    inherit package;
  };

  home-manager.sharedModules = [
    {
      programs.emacs = {
        enable = true;
        inherit package;
      };
    }
  ];
}
