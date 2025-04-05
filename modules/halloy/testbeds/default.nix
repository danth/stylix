{ pkgs, ... }:

let
  package = pkgs.halloy;
in
{
  stylix.testbed.ui.application = {
    name = "org.squidowl.halloy";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
