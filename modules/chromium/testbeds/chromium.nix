{ pkgs, ... }:

let
  package = pkgs.chromium;
in
{
  stylix.testbed.ui.application = {
    name = "chromium-browser";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
