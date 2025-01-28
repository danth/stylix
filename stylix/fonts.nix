{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.stylix.fonts;

  fontType = lib.types.submodule {
    options = {
      package = lib.mkOption {
        description = "Package providing the font.";
        type = lib.types.package;
      };

      name = lib.mkOption {
        description = "Name of the font within the package.";
        type = lib.types.str;
      };
    };
  };

  unitsDoc = ''
    This is measured in [points](https://en.wikipedia.org/wiki/Point_(typography)).
    In a computing context, there should be 72 points per inch.

    [The CSS specification](https://drafts.csswg.org/css-values/#absolute-lengths)
    says there should be 96 reference pixels per inch. These are not always
    equal to one physical pixel, but it means CSS uses a fixed ratio of 3
    points to every 4 pixels, which is sometimes useful.

    Other programs might measure in physical pixels, which makes the previously
    mentioned conversion invalid.

    The measurements in inches are likely to be incorrect unless you've
    [carefully configured your DPI](https://linuxreviews.org/HOWTO_set_DPI_in_Xorg).
  '';

in
{
  options.stylix.fonts = {
    serif = lib.mkOption {
      description = "Serif font.";
      type = fontType;
      default = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
    };

    sansSerif = lib.mkOption {
      description = "Sans-serif font.";
      type = fontType;
      default = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };
    };

    monospace = lib.mkOption {
      description = "Monospace font.";
      type = fontType;
      default = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans Mono";
      };
    };

    emoji = lib.mkOption {
      description = "Emoji font.";
      type = fontType;
      default = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };

    sizes = {
      desktop = lib.mkOption {
        description = ''
          The font size used in window titles/bars/widgets elements of the
          desktop.

          ${unitsDoc}
        '';
        type = with lib.types; (either ints.unsigned float);
        default = 10;
      };

      applications = lib.mkOption {
        description = ''
          The font size used by applications.

          ${unitsDoc}
        '';
        type = with lib.types; (either ints.unsigned float);
        default = 12;
      };

      terminal = lib.mkOption {
        description = ''
          The font size for terminals/text editors.

          ${unitsDoc}
        '';
        type = with lib.types; (either ints.unsigned float);
        default = cfg.sizes.applications;
      };

      popups = lib.mkOption {
        description = ''
          The font size for notifications/popups and in general overlay
          elements of the desktop.

          ${unitsDoc}
        '';
        type = with lib.types; (either ints.unsigned float);
        default = cfg.sizes.desktop;
      };
    };

    packages = lib.mkOption {
      description = ''
        A list of all the font packages that will be installed.
      '';
      type = lib.types.listOf lib.types.package;
      readOnly = true;
    };
  };

  config = lib.mkIf config.stylix.enable {
    stylix.fonts.packages = [
      cfg.monospace.package
      cfg.serif.package
      cfg.sansSerif.package
      cfg.emoji.package
    ];
  };
}
