{ lib, config, ... }:
{
  options.stylix.testbed.enable = lib.mkOption {
    type = lib.types.bool;
    default = true;
    example = lib.literalExpression "lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.discord";
    description = ''
      Whether to enable this testbed.

      The testbed will not be included as a flake output if set to false.

      > [!CAUTION]
      >
      > This option can only access `lib` and `pkgs` inputs. Attempting to
      > read other inputs, like `config` or `options`, will cause the
      > testbed evaluation to fail.
      >
      > This is a performance-driven restriction, as noted in `isEnabled`.
    '';
  };

  config.assertions = [
    {
      assertion = config.stylix.testbed.enable;
      message = "Building a disabled testbed. This testbed should have been filtered out!";
    }
  ];
}
