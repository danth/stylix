{
  pkgs,
  lib,
  inputs,
  ...
}:
builtins.mapAttrs
  (
    _: value:
    if (builtins.typeOf value == "lambda") then
      (value {
        inherit pkgs;
        lib = pkgs.lib.extend (
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
      path: kind:
      lib.optionalAttrs (kind == "directory") {
        ${path} = import "${inputs.self}/modules/${path}/meta.nix";
      }
    ) (builtins.readDir "${inputs.self}/modules")
  )
