{ pkgs, ... }:

let
  package = pkgs.gnome-text-editor;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "org.gnome.TextEditor";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
