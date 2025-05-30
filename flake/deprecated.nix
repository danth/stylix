{
  lib,
  self,
  config,
  ...
}:
let
  cfg = config.stylix;

  # Produces a perSystem module defining an alias
  mkPerSystemAliasModule =
    {
      output,
      since,
      until,
      old,
      new,
    }:
    { config, system, ... }:
    {
      ${output} = lib.mkIf (!lib.trivial.oldestSupportedReleaseIsAtLeast until) {
        ${old} =
          lib.warnIf (since != null -> lib.trivial.oldestSupportedReleaseIsAtLeast since)
            "stylix: flake output `${output}.${system}.${lib.strings.escapeNixIdentifier old}` has been renamed to `${output}.${system}.${lib.strings.escapeNixIdentifier new}`."
            config.${output}.${new};
      };
    };
in
{
  options.stylix = {
    perSystemAliases = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            output = lib.mkOption {
              type = lib.types.str;
              description = "The per-system attribute in which to define the alias.";
            };
            old = lib.mkOption {
              type = lib.types.str;
              description = "The name of the alias.";
            };
            new = lib.mkOption {
              type = lib.types.str;
              description = "The name of the alias target.";
            };
            since = lib.mkOption {
              type = with lib.types; nullOr ints.unsigned;
              default = null;
              description = ''
                Warn only once the specified release is the oldest supported nixpkgs release.

                If `null`, the alias will always warn.
              '';
            };
            until = lib.mkOption {
              type = lib.types.ints.unsigned;
              description = ''
                Create the alias only until the specified release is the oldest supported nixpkgs release.

                The alias spec can be safely removed after this release.
              '';
            };
          };
        }
      );
      default = [ ];
      description = "A list of per-system aliases.";
    };
  };

  config = {
    # NOTE: the `flake` submodule has a `lazyAttrsOf` freeform type.
    #
    # This means a `mkIf false` definition will not omit the attr, because
    # `lazyAttrsOf` adds an "empty value" stub throwing "used but not defined".
    #
    # Therefore, instead of doing `flake.foo = mkIf` you should do `flake = mkIf (…) { foo = …; }`.
    # If you need multiple definitions with different conditions, use `flake = mkMerge [ … ]`.

    # Drop this alias after 26.05
    flake = lib.mkIf (!lib.oldestSupportedReleaseIsAtLeast 2605) {
      homeManagerModules = builtins.warn "stylix: flake output `homeManagerModules` has been renamed to `homeModules`" self.homeModules;
    };

    stylix.perSystemAliases = [
      {
        output = "packages";
        old = "docs";
        new = "doc";
        until = 2511;
      }
    ];

    perSystem = lib.mkIf (cfg.perSystemAliases != [ ]) {
      imports = map mkPerSystemAliasModule cfg.perSystemAliases;
    };
  };
}
