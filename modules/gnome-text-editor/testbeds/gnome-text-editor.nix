{ pkgs, ... }:

let
  package = pkgs.gnome-text-editor;
in
{
  stylix.testbed.ui.application = {
    name = "org.gnome.TextEditor";
    inherit package;
  };

  environment.systemPackages = [ package ];
}
