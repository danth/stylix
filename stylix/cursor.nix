{ pkgs, lib, ... }:

{
  options.stylix.cursor = {
    name = lib.mkOption {
      description = "The cursor name within the package.";
      type = lib.types.str;
      default = "Vanilla-DMZ";
    };
    package = lib.mkOption {
      description = "Package providing the cursor theme.";
      type = lib.types.package;
      default = pkgs.vanilla-dmz;
    };
    size = lib.mkOption {
      description = "The cursor size.";
      type = lib.types.int;
      default = 32;
    };
  };
}
