{ lib }:

# string -> [ path ]
# List include path for either nixos modules or hm modules
for:
builtins.concatLists (
  lib.mapAttrsToList (
    path: kind:
    let
      file = ../modules + "${path}/${for}.nix";
    in
    lib.optional (kind == "directory" && builtins.pathExists file) file
  ) (builtins.readDir ../modules)
)
