{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.stylix.fonts;

  mkFontOptions =
    {
      fontName,
      displayName,
      package,
    }:
    {
      package = lib.mkPackageOption pkgs package { } // {
        description = "Package providing the ${displayName} font.";
      };

      name = lib.mkOption {
        type = lib.types.str;
        description = "Name of the ${displayName} font.";
        default = fontName;
      };
    };

in
{
  options.stylix.fonts = {
    serif = mkFontOptions {
      displayName = "Serif";
      fontName = "DejaVu Serif";
      package = "dejavu_fonts";
    };

    sansSerif = mkFontOptions {
      displayName = "Sans-serif";
      fontName = "DejaVu Sans";
      package = "dejavu_fonts";
    };

    monospace = mkFontOptions {
      displayName = "Monospace";
      fontName = "DejaVu Sans Mono";
      package = "dejavu_fonts";
    };

    emoji = mkFontOptions {
      displayName = "Emoji";
      fontName = "Noto Color Emoji";
      package = "noto-fonts-color-emoji";
    };

    sizes =
      let
        mkFontSizeOption =
          { default, target }:
          lib.mkOption {
            inherit default;

            description = ''
              The font size used for ${target}.

              This is measured in [points](https://en.wikipedia.org/wiki/Point_(typography)).
              In a computing context, there should be 72 points per inch.

              [The CSS specification](https://drafts.csswg.org/css-values/#absolute-lengths)
              says there should be 96 reference pixels per inch. This means CSS
              uses a fixed ratio of 3 points to every 4 pixels, which is
              sometimes useful. However, reference pixels might not correspond
              to physical pixels, so this conversion may be invalid for other
              applications.

              The measurements given in inches are likely to be incorrect
              unless you've
              [manually set your DPI](https://linuxreviews.org/HOWTO_set_DPI_in_Xorg).
            '';

            type = with lib.types; either ints.unsigned float;
          };
      in
      {
        desktop = mkFontSizeOption {
          target = "window titles, status bars, and other general elements of the desktop";
          default = 10;
        };

        applications = mkFontSizeOption {
          target = "applications";
          default = 12;
        };

        terminal = mkFontSizeOption {
          target = "terminals and text editors";
          default = cfg.sizes.applications;
        };

        popups = mkFontSizeOption {
          target = "notifications, popups, and other overlay elements of the desktop";
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
