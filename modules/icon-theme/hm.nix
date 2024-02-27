{ pkgs, lib, config, ... }:

let
  cfg = config.stylix.targets.iconTheme;
  pythonEnv = pkgs.python3.withPackages
    (ps: with ps; [
      colormath
      tqdm
      pillow
    ]);
  recolorScript = with cfg.recolor; ''
    ${pythonEnv}/bin/python ${./recolor.py} --src $out/share/icons \
       --smooth '${toString smooth}' \
       --foreground-threshold ${foregroundThreshold} \
       ${if isNull accentSaturation then "" else "--accent-saturation ${accentSaturation}"} \
       ${if isNull accentSaturationMultiply then "" else "--accent-saturation-multiply ${accentSaturationMultiply}"} \
       ${if isNull accentLight then "" else "--accent-light ${accentLight}"} \
       ${if isNull accentLightMultiply then "" else "--accent-light-multiply ${accentLightMultiply}"} \
       ${if isNull foregroundSaturation then "" else "--foreground-saturation ${foregroundSaturation}"} \
       ${if isNull foregroundSaturationMultiply then "" else "--foreground-saturation-multiply ${foregroundSaturationMultiply}"} \
       ${if isNull foregroundLight then "" else "--foreground-light ${foregroundLight}"} \
       ${if isNull foregroundLightMultiply then "" else "--foreground-light-multiply ${foregroundLightMultiply}"} \
       ${if cfg.recolor.mode == "monochrome" then
         "--monochrome '${builtins.concatStringsSep "," cfg.recolor.colors}'"
       else
         "--palette ''${builtins.concatStringsSep "," cfg.recolor.colors}''"}
  '';
in
{
  options.stylix.targets.iconTheme = {
    enable = config.lib.stylix.mkEnableTarget "the icon theme" true;
    name = lib.mkOption {
      description = "The icon theme name";
      type = lib.types.str;
      default = "Zafiro-icons-Dark";
    };
    package = lib.mkOption {
      description = "The icon theme package";
      type = lib.types.package;
      default = pkgs.zafiro-icons;
    };
    recolor = {
      enable = lib.mkOption {
        description = "Whether to recolor the icon theme colors.";
        type = lib.types.bool;
        default = true;
      };
      enableForParentThemes = lib.mkOption {
        description = "Whether to recolor the parent icon theme colors.";
        type = lib.types.bool;
        default = true;
      };
      mode = lib.mkOption {
        description = ''The mode to use:
          monochrome = A monochromatic variant, colored by appropriate shades of the provided base color.
          palette = A multichromatic variant, where all colors are replaced by their nearest perceived equivalent that adheres to the provided color palette.
        '';
        type = lib.types.enum [ "monochrome" "palette" ];
        default = "palette";
      };
      smooth = lib.mkOption {
        description = "#TODO explain this? kinda hard, showing some pictures will be easier.";
        type = lib.types.bool;
        default = true;
      };
      foregroundThreshold = lib.mkOption {
        description = ''This speicifes the max distance from white for which the color will be considered foreground.
          anything smaller and the 'foreground' modifications will be used, anything larger
          and the accent modifications will be used.
          Values between 0.0 and 1.0.
        '';
        type = lib.types.str;
        default = "0.85";
      };
      accentSaturation = lib.mkOption {
        description = ''Override icon accent colors saturation with custom value.
          Values between 0.0 and 1.0.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      accentSaturationMultiply = lib.mkOption {
        description = ''Multiply icon accent colors saturation by value.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      accentLight = lib.mkOption {
        description = ''Override icon accent colors light with custom value.
          Values between 0.0 and 1.0.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      accentLightMultiply = lib.mkOption {
        description = ''Multiply icon accent colors light by value.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      foregroundSaturation = lib.mkOption {
        description = ''Override icon foreground colors saturation with custom value.
          Values between 0.0 and 1.0.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      foregroundSaturationMultiply = lib.mkOption {
        description = ''Multiply icon foreground colors saturation by value.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      foregroundLight = lib.mkOption {
        description = ''Override icon foreground colors light with custom value.
          Values between 0.0 and 1.0.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      foregroundLightMultiply = lib.mkOption {
        description = ''Multiply icon foreground colors light by value.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      colors = lib.mkOption {
        description = "The color list";
        type = lib.types.listOf (lib.types.str);
        default =
          if cfg.recolor.mode == "monochrome" then
            with config.lib.stylix.colors.withHashtag; [
              base08
              base09
              base0A
              base0B
              base0C
              base0D
              base0E
              base0F
            ]
          else
            with config.lib.stylix.colors.withHashtag; [
              base00
              base01
              base02
              base03
              base04
              base05
              base06
              base07
              base08
              base09
              base0A
              base0B
              base0C
              base0D
              base0E
              base0F
            ];
      };
    };
  };

  config = cfg.enable {
    gtk = {
      enable = true;
      iconTheme = lib.mkMerge [
        {
          name = cfg.name;
        }

        (lib.mkIf (!cfg.recolor.enable) {
          package = cfg.package;
        })
        (lib.mkIf (cfg.recolor.enable) {
          package = cfg.package.overrideAttrs
            (oldAttrs: rec {
              propagatedBuildInputs =
                if cfg.recolor.enableForParentThemes then
                  builtins.map
                    (parentIconTheme: parentIconTheme.overrideAttrs (parentIconThemeOldAttrs: rec {
                      postInstall = recolorScript + (parentIconThemeOldAttrs.postInstall or "");
                    }))
                    # For some reason patching gnome-icon-theme takes forever
                    (builtins.filter (parentIconTheme: parentIconTheme.pname != "gnome-icon-theme") oldAttrs.propagatedBuildInputs)
                else oldAttrs.propagatedBuildInputs;

              postInstall = recolorScript + (oldAttrs.postInstall or "");
            });
        })
      ];
    };
  };
}
