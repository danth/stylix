{ pkgs, ... }:
let
  package = pkgs.blender;
in
{
  stylix.testbed.ui.application = {
    name = "blender";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
