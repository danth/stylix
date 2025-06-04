{ lib, config, ... }:
{
  perSystem.options.stylix.aliases = lib.mkOption {
    type = lib.types.listOf (
      lib.types.submodule {
        options = {
          output = lib.mkOption {
            type = lib.types.str;
            description = ''
              The per-system attribute in which to define the alias.
            '';
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
              Warn only once the specified release is the oldest supported
              nixpkgs release.

              If `null`, the alias will always warn.
            '';
          };
          until = lib.mkOption {
            type = lib.types.ints.unsigned;
            description = ''
              Create the alias only until the specified release is the oldest
              supported nixpkgs release.

              The alias spec can be safely removed after this release.
            '';
          };
        };
      }
    );
    default = [ ];
    description = "A list of per-system aliases.";
  };

  # Transpose per-system aliases to the top-level
  flake = lib.mkMerge (
    lib.mapAttrsToList (
      system: cfg:
      let
        # Produces config definition for an alias
        mkAlias =
          {
            output,
            since,
            until,
            old,
            new,
          }:
          let
            paths = builtins.mapAttrs (_: attr: [
              output
              system
              attr
            ]) { inherit old new; };
            names = builtins.mapAttrs (_: lib.showAttrPath) paths // {
              until = lib.pipe until [
                builtins.toString
                (builtins.match "([[:digit:]]{2})([[:digit:]]{2})")
                (lib.concatStringsSep ".")
              ];
            };
          in
          lib.mkIf (!lib.oldestSupportedReleaseIsAtLeast until) (
            lib.attrsets.setAttrByPath paths.old (
              lib.warnIf (since != null -> lib.oldestSupportedReleaseIsAtLeast since)
                "stylix: flake output `${names.old}` has been renamed to `${names.new}` and will be removed after ${names.until}."
                (cfg.${output}.${new} or (throw "stylix: flake alias not found: ${names.new}"))
            )
          );
      in
      lib.mkMerge (map mkAlias cfg.stylix.aliases)
    ) config.allSystems
  );
}
