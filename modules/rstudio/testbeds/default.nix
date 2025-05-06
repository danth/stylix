{ pkgs, ... }:

let
  package = pkgs.rstudio;
in
{
  stylix.testbed.ui = {
    application = {

      name = "rstudio";
      inherit package;
    };
    command.text = "rstudio";
  };

  environment.systemPackages = [ package ];
}
