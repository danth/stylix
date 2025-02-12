{ pkgs, ... }:

let
  package = pkgs.chromium;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "chromium-browser";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
