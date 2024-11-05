{ pkgs, lib, ... }:

{
    options.stylix.iconTheme = {
        package = lib.mkOption {
            description = "Package providing the icon theme.";
            type = lib.types.nullOrpackage;
            default = null;
        };
        light = lib.mkOption {
            description = "Light icon theme name.";
            type = lib.types.str;
            default = null;
        };
        dark = lib.mkOption {
            description = "Dark icon theme name.";
            type = lib.types.str;
            default = null;
        };
    };
}
