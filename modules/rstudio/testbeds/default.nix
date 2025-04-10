{ pkgs, ... }:

let
  package = pkgs.rstudio;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "rstudio";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
