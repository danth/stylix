{
  pkgs,
  lib,
  ...
}@args:
let
  ghIds = lib.concatMapAttrs (name: value: {
    "modules/${name}" = builtins.concatMap (m: m.github or [ ]) value.maintainers;
  }) (import ./meta.nix args);
in
pkgs.writeText "get-maintainers" (builtins.toJSON { inherit ghIds; })
