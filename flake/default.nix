{ inputs, ... }:
{
  imports = [
    inputs.flake-parts.flakeModules.partitions
    ./modules.nix
  ];

  # Define flake partitions
  # Each has a `module`, assigned to the partition's submodule,
  # and an `extraInputsFlake`, used for its inputs.
  # See https://flake.parts/options/flake-parts-partitions.html
  partitions = {
    dev = {
      module = ./dev;
      extraInputsFlake = ./dev;
    };
  };

  # Specify which outputs are defined by which partitions
  partitionedAttrs = {
    checks = "dev";
    devShells = "dev";
    formatter = "dev";
  };

  perSystem =
    { pkgs, ... }:
    {
      packages.palette-generator = pkgs.callPackage ../palette-generator { };
    };
}
