args@{ lib, ... }:

lib.mkRemovedOptionModule
  [
    "stylix"
    "targets"
    "gnome-text-editor"
    "enable"
  ]
  "Use the Home Manager target of the same name or `stylix.targets.gtksourceview.enable' instead."
  args
