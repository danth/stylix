{ pkgs, ... }:

let
  package = pkgs.vesktop;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "vesktop";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
