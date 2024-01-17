{ pkgs, config, lib, ... } @ args:

with lib;

let
  cfg = config.stylix.fonts;

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

  # A best hope mix of
  #
  # - lib.types.listOf
  # - lib.types.nonEmptyListOf
  # - lib.types.coercedTo
  #
  # My understanding is shallow and my hopes are high.
  coercedToNonEmptyListOf = elemType: types.mkOptionType ({
    name = "coercedToNonEmptyList of ${types.optionDescriptionPhrase (class: class == "noun" || class == "composite") elemType}";
    descriptionClass = "composite";
    check = x: if builtins.isList x then x != [ ] else elemType.check x;
    merge = loc: defs:
      let coerceVal = val: if builtins.isList val then val else [ val ];
      in
      builtins.map (x: x.value) (builtins.filter (x: x ? value)
        (builtins.concatLists
          (lists.imap1
            (n: def:
              lists.imap1
                (m: def':
                  (mergeDefinitions
                    (loc ++ [ "[definition ${toString n}-entry ${toString m}]" ])
                    elemType
                    [{ inherit (def) file; value = def'; }]
                  ).optionalValue
                )
                def.value
            )
            (builtins.map (def: def // { value = coerceVal def.value; }) defs))));
    emptyValue = { }; # unset
    getSubOptions = prefix: elemType.getSubOptions (prefix ++ [ "*" ]);
    getSubModules = elemType.getSubModules;
    substSubModules = m: coercedToNonEmptyListOf (elemType.substSubModules m);
    functor = (defaultFunctor name) // { wrapped = elemType; };
    nestedTypes.elemType = elemType;
  });

  fontList = coercedToNonEmptyListOf fontType;
in
{
  options.stylix.fonts = {
    serif = mkOption {
      description = "Serif font.";
      type = fontList;
      default = fromOs [ "fonts" "serif" ] [{
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      }];
    };

    sansSerif = mkOption {
      description = "Sans-serif font.";
      type = fontList;
      default = fromOs [ "fonts" "sansSerif" ] [{
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      }];
    };

    monospace = mkOption {
      description = "Monospace font.";
      type = fontList;
      default = fromOs [ "fonts" "monospace" ] [{
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans Mono";
      }];
    };

    emoji = mkOption {
      description = "Emoji font.";
      type = fontList;
      default = fromOs [ "fonts" "emoji" ] [{
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      }];
    };

    sizes = {
      desktop = mkOption {
        description = ''
          The font size used in window titles/bars/widgets elements of
          the desktop.
        '';
        type = types.ints.unsigned;
        default = fromOs [ "fonts" "sizes" "desktop" ] 10;
      };

      applications = mkOption {
        description = ''
          The font size used by applications.
        '';
        type = types.ints.unsigned;
        default = fromOs [ "fonts" "sizes" "applications" ] 12;
      };

      terminal = mkOption {
        description = ''
          The font size for terminals/text editors.
        '';
        type = types.ints.unsigned;
        default = fromOs [ "fonts" "sizes" "terminal" ] cfg.sizes.applications;
      };

      popups = mkOption {
        description = ''
          The font size for notifications/popups and in general overlay
          elements of the desktop.
        '';
        type = types.ints.unsigned;
        default = fromOs [ "fonts" "sizes" "popups" ] cfg.sizes.desktop;
      };
    };

    packages = mkOption {
      description = ''
        A list of all the font packages that will be installed.
      '';
      type = types.listOf types.package;
      readOnly = true;
    };
  };

  config = {
    stylix.fonts.packages =
      (lib.catAttrs "package" cfg.monospace)
      ++ (lib.catAttrs "package" cfg.serif)
      ++ (lib.catAttrs "package" cfg.sansSerif)
      ++ (lib.catAttrs "package" cfg.emoji);
  };
}
