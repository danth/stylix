{ lib, pkgs, ... }:

let
  package = pkgs.blender;
in

{
  stylix.testbed.application = {
    enable = true;
    name = "blender";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
