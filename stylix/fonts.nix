{ pkgs, config, lib, ... }:

with lib;

let
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
      default = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
    };

    sansSerif = mkOption {
      description = "Sans-serif font.";
      type = fontType;
      default = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };
    };

    monospace = mkOption {
      description = "Monospace font.";
      type = fontType;
      default = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans Mono";
      };
    };

    emoji = mkOption {
      description = "Emoji font.";
      type = fontType;
      default = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };
  };
}
