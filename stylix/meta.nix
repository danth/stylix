{
  pkgs,
  lib,
  inputs,
  ...
}:
lib.forEach (import ../autoload.nix { inherit lib inputs; } "meta") (
  attrs:
  attrs {
    inherit pkgs;
    lib = pkgs.lib.extend (
      _: _: { stylix.maintainers = import ./maintainers.nix; }
    );
  }
)
