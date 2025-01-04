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
          The font size (in pt) used in window titles/bars/widgets elements of
          the desktop.
        '';
        type = with lib.types; (either ints.unsigned float);
        default = 10;
      };

      applications = lib.mkOption {
        description = ''
          The font size (in pt) used by applications.
        '';
        type = with lib.types; (either ints.unsigned float);
        default = 12;
      };

      terminal = lib.mkOption {
        description = ''
          The font size (in pt) for terminals/text editors.
        '';
        type = with lib.types; (either ints.unsigned float);
        default = cfg.sizes.applications;
      };

      popups = lib.mkOption {
        description = ''
          The font size (in pt) for notifications/popups and in general overlay
          elements of the desktop.
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
