{ lib, config, ... }:

{
  options.stylix.cursor = lib.mkOption {
    description = ''
      Attributes defining the systemwide cursor. Set either all or none of
      these attributes.
    '';
    type = lib.types.nullOr (
      lib.types.submodule {
        options = {
          name = lib.mkOption {
            description = "The cursor name within the package.";
            type = lib.types.nullOr lib.types.str;
            default = null;
          };
          package = lib.mkOption {
            description = "Package providing the cursor theme.";
            type = lib.types.nullOr lib.types.package;
            default = null;
          };
          size = lib.mkOption {
            description = "The cursor size.";
            type = lib.types.nullOr lib.types.int;
            default = null;
          };
        };
      }
    );
    default = null;
  };
  config.assertions =
    let
      inherit (config.stylix) cursor;
    in
    [
      {
        assertion =
          cursor == null
          || cursor.name != null && cursor.package != null && cursor.size != null;
        message = ''
          stylix: `stylix.cursor` is only partially defined. Set either none or
          all of the `stylix.cursor` options.
        '';
      }
    ];
}
