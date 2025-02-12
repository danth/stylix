{ pkgs, ... }:

let
  package = pkgs.halloy;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "org.squidowl.halloy";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
