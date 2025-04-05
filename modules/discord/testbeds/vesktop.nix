{ pkgs, ... }:

let
  package = pkgs.vesktop;
in
{
  stylix.testbed.ui.application = {
    name = "vesktop";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
