{ pkgs, config, lib, ... } @ args:

with lib;

let

  fromOs = import ./fromos.nix { inherit lib args; };

  fontType = types.submodule {
    options = {
      package = mkOption {
        description = "Package providing the font.";
        type = types.package;
      };

      name = mkOption {
        description = "Name of the font within the package.";
        type = types.str;
      };
    };
  };

in {
  options.stylix.fonts = {
    serif = mkOption {
      description = "Serif font.";
      type = fontType;
      default = fromOs [ "fonts" "serif" ] {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
    };

    sansSerif = mkOption {
      description = "Sans-serif font.";
      type = fontType;
      default = fromOs [ "fonts" "sansSerif" ] {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };
    };

    monospace = mkOption {
      description = "Monospace font.";
      type = fontType;
      default = fromOs [ "fonts" "monospace" ] {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans Mono";
      };
    };

    emoji = mkOption {
      description = "Emoji font.";
      type = fontType;
      default = fromOs [ "fonts" "emoji" ] {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };

    sizes = let
      mkFontSize = default: description: mkOption {
        inherit description default;
        type = types.ints.unsigned;
      };
    in {
      desktop = mkFontSize 10 ''
        The font size used in window titles/bars/widgets elements of the desktop.
      '';
      applications = mkFontSize 12 ''
        The font size used by applications.
      '';
      terminal = mkFontSize config.stylix.fonts.sizes.applications ''
        The font size for terminals/text editors.
      '';
      popups = mkFontSize config.stylix.fonts.sizes.desktop ''
        The font size for notifications/popups and in general overlay elements of the desktop.
      '';
    };
  };
}
