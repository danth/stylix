{
  lib,
  pkgs,
  inputs,
  writeText,
}:
let
  ghIds = lib.mapAttrs' (
    name: value:
    lib.nameValuePair "modules/${name}" (
      builtins.concatMap (m: [ m.github ]) value.maintainers
    )
  ) (import ./meta.nix { inherit lib pkgs inputs; });
in
writeText "get-maintainers" (builtins.toJSON { inherit ghIds; })
