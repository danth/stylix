{
  pkgs,
  lib,
  ...
}@args:
let
  ghIds = lib.mapAttrs' (
    name: value:
    lib.nameValuePair "modules/${name}" (
      builtins.concatMap (m: [ m.github ]) value.maintainers
    )
  ) (import ./meta.nix args);
in
pkgs.writeText "get-maintainers" (builtins.toJSON { inherit ghIds; })
