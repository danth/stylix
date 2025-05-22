{
  lib,
  self,
  inputs,
  partitionStack,
  ...
}:
{
  imports = [
    inputs.flake-parts.flakeModules.partitions
    ./deprecated.nix
    ./modules.nix
  ];

  partitions.dev = {
    module = ./dev;
    extraInputsFlake = ./dev;
  };

  partitionedAttrs = {
    checks = "dev";
    devShells = "dev";
    formatter = "dev";
  };

  perSystem =
    { pkgs, ... }:
    {
      packages = lib.mkMerge [
        {
          palette-generator = pkgs.callPackage "${self}/palette-generator" { };
        }
        # For any output attrs normally defined by the root flake configuration,
        # any exceptions must be manually propagated from the `dev` partition.
        #
        # NOTE: Attrs should be explicitly propagated at the deepest level.
        # Otherwise the partition won't be lazy, making it pointless.
        # E.g. propagate `packages.${system}.foo` instead of `packages.${system}`
        # See: https://github.com/hercules-ci/flake-parts/issues/258
        (lib.optionalAttrs (partitionStack == [ ]) {
          # FIXME: propagate testbeds
        })
      ];
    };
}
