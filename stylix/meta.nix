{
  pkgs,
  lib,
}:
builtins.mapAttrs
  (
    _: value:
    if builtins.isFunction value then
      (value {
        inherit pkgs;
        lib = lib.extend (
          _: prev: {
            maintainers = lib.attrsets.unionOfDisjoint prev.maintainers (import ./maintainers.nix);
          }
        );
      })
    else
      value
  )
  (
    lib.concatMapAttrs (
      target: kind:
      lib.optionalAttrs (kind == "directory") {
        ${target} = import ../modules/${target}/meta.nix;
      }
    ) (builtins.readDir ../modules)
  )
