{ lib, ... }:

{
  options.stylix.cursor = lib.mkOption {
    description = "Attributes defining the systemwide cursor.";
    type = lib.types.nullOr (
      lib.types.submodule {
        options = {
          name = lib.mkOption {
            description = "The cursor name within the package.";
            type = lib.types.str;
          };
          package = lib.mkOption {
            description = "Package providing the cursor theme.";
            type = lib.types.package;
          };
          size = lib.mkOption {
            description = "The cursor size.";
            type = lib.types.int;
          };
        };
      }
    );
    default = null;
  };
}
