{ pkgs, ... }:

let
  package = pkgs.gedit;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "org.gnome.gedit";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
