{
  lib,
  self,
  ...
}:
{
  imports = [
    ./per-system-option.nix
  ];

  # NOTE: the `flake` submodule has a `lazyAttrsOf` freeform type.
  #
  # This means a `mkIf false` definition will not omit the attr, because
  # `lazyAttrsOf` adds an "empty value" stub throwing "used but not defined".
  #
  # Therefore, instead of doing `flake.foo = mkIf` you should do `flake = mkIf (…) { foo = …; }`.
  # If you need multiple definitions with different conditions, use `flake = mkMerge [ … ]`.

  # Drop this alias after 26.05
  flake = lib.mkIf (!lib.oldestSupportedReleaseIsAtLeast 2605) {
    homeManagerModules = builtins.warn "stylix: flake output `homeManagerModules` has been renamed to `homeModules` and will be removed after 26.05." self.homeModules;
  };

  perSystem.stylix.aliases = [
    {
      output = "apps";
      old = "docs";
      new = "doc";
      until = 2511;
    }
    {
      output = "packages";
      old = "docs";
      new = "doc";
      until = 2511;
    }
  ];
}
