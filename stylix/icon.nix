{ lib, ... }:

{
  options.stylix.iconTheme = {
    enable = lib.mkOption {
      description = "enable/disable icon theming.";
      type = lib.types.bool;
      default = false;
    };
    package = lib.mkOption {
      description = "Package providing the icon theme.";
      type = lib.types.nullOr lib.types.package;
      default = null;
    };
    light = lib.mkOption {
      description = "Light icon theme name.";
      type = lib.types.nullOr lib.types.str;
      default = null;
    };
    dark = lib.mkOption {
      description = "Dark icon theme name.";
      type = lib.types.nullOr lib.types.str;
      default = null;
    };
  };
}
