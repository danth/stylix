{ lib }:

lib.pipe ./graphicalEnvironments [
  builtins.readDir
  (lib.mapAttrsToList (name: _: lib.removeSuffix ".nix" name))
]
