{ pkgs, lib, ... }:

with lib;

{
    options.stylix.cursor = {
        name = mkOption {
            description = "The cursor name within the package.";
            type = types.str;
            default = "Vanilla-DMZ";
        };
        package = mkOption {
            description = "Package providing the cursor theme.";
            type = types.package;
            default = pkgs.vanilla-dmz;
        };
        size = mkOption {
            description = "The cursor size.";
            type = types.int;
            default = 32;
        };
    };
}
