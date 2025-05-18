{ lib, inputs }:

# string -> [ path ]
# List include path for either nixos modules or hm modules
for:
builtins.concatLists (
  lib.mapAttrsToList (
    path: kind:
    let
      file = "${inputs.self}/modules/${path}/${for}.nix";
    in
    lib.optional (kind == "directory" && builtins.pathExists file) file
  ) (builtins.readDir "${inputs.self}/modules")
)
