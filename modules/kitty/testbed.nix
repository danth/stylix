{ pkgs, ... }:

let
  package = pkgs.kitty;

in
{
  stylix.testbed.application = {
    enable = true;
    name = "kitty";
    inherit package;
  };

  home-manager.sharedModules = [
    {
      programs.kitty = {
        enable = true;
        inherit package;
      };
    }
  ];
}
