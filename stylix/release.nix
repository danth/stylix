{ lib, ... }:
{
  options.stylix = {
    release = lib.mkOption {
      description = "The version of NixOS that Stylix is built to support";
      default = "25.11";
      internal = true;
      readOnly = true;
    };
    enableReleaseChecks = lib.mkOption {
      description = ''
        Whether to check that the Stylix release matches the releases of
        NixOS, Home Manager, and nix-darwin. Checks are only performed if the
        component in question is used.

        If this option is enabled and a mismatch is detected, a warning will be
        printed when the user configuration is being built.
      '';
      type = lib.types.bool;
      default = true;
    };
  };
}
