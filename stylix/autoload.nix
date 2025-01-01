{ lib, inputs }:

# string -> [ path ]
# List include path for either nixos modules or hm modules
for:
builtins.concatLists (
  lib.mapAttrsToList (
    path: kind:
    if kind == "directory" then
      let
        file = "${inputs.self}/modules/${path}/${for}.nix";
      in
      if builtins.pathExists file then [ file ] else [ ]
    else
      [ ]
  ) (builtins.readDir "${inputs.self}/modules")
)
