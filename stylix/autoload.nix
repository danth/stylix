{ lib, inputs }:

# string -> [ path ]
# List include path for either nixos modules or hm modules
for:
builtins.concatLists (
  lib.mapAttrsToList (
    path: kind:
    let
      file = "${inputs.self}/modules/${path}/${for}.nix";
      module = import file;
      # Assume that a function-module whose first arg doesn't have any de-
      # structured attrs is actually expecting `mkTarget` as its first arg;
      #
      # `mkTarget` cannot be distribtued normally through the module system
      # due to issues of infinite recursion
      expectsMkTarget =
        builtins.isFunction module && builtins.functionArgs module == { };
    in
    lib.optional (kind == "directory" && builtins.pathExists file) (
      if expectsMkTarget then
        {
          key = file;
          _file = file;
          imports = [ (module (import ./mk-target.nix)) ];
        }
      else
        file
    )
  ) (builtins.readDir "${inputs.self}/modules")
)
