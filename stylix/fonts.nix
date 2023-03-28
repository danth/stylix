{ pkgs, config, lib, ... } @ args:

with lib;

let
  cfg = config.stylix.fonts;

  fromOs = import ./fromos.nix { inherit lib args; };

  fontType = types.submodule {
    options = {
      package = mkOption {
        description = mdDoc "Package providing the font.";
        type = types.package;
      };

      name = mkOption {
        description = mdDoc "Name of the font within the package.";
        type = types.str;
      };
    };
  };

in {
  options.stylix.fonts = {
    serif = mkOption {
      description = mdDoc "Serif font.";
      type = fontType;
      default = fromOs [ "fonts" "serif" ] {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
    };

    sansSerif = mkOption {
      description = mdDoc "Sans-serif font.";
      type = fontType;
      default = fromOs [ "fonts" "sansSerif" ] {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };
    };

    monospace = mkOption {
      description = mdDoc "Monospace font.";
      type = fontType;
      default = fromOs [ "fonts" "monospace" ] {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans Mono";
      };
    };

    emoji = mkOption {
      description = mdDoc "Emoji font.";
      type = fontType;
      default = fromOs [ "fonts" "emoji" ] {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };

    sizes = {
      desktop = mkOption {
        description = mdDoc ''
          The font size used in window titles/bars/widgets elements of
          the desktop.
        '';
        type = types.ints.unsigned;
        default = fromOs [ "fonts" "sizes" "desktop" ] 10;
      };

      applications = mkOption {
        description = mdDoc ''
          The font size used by applications.
        '';
        type = types.ints.unsigned;
        default = fromOs [ "fonts" "sizes" "applications" ] 12;
      };

      terminal = mkOption {
        description = mdDoc ''
          The font size for terminals/text editors.
        '';
        type = types.ints.unsigned;
        default = fromOs [ "fonts" "sizes" "terminal" ] cfg.sizes.applications;
      };

      popups = mkOption {
        description = mdDoc ''
          The font size for notifications/popups and in general overlay
          elements of the desktop.
        '';
        type = types.ints.unsigned;
        default = fromOs [ "fonts" "sizes" "popups" ] cfg.sizes.desktop;
      };
    };
  };
}
