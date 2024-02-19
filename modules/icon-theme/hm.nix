{ pkgs, lib, config, ... }:

let
  cfg = config.stylix.targets.iconTheme;
  pythonEnv = pkgs.python3.withPackages
    (ps: with ps; [
      colormath
      tqdm
      pillow
    ]);
in
{
  options.stylix.targets.iconTheme = {
    enable = config.lib.stylix.mkEnableTarget "the icon theme" true;
    name = lib.mkOption {
      description = "The icon theme name";
      type = lib.types.str;
      default = "Numix-Circle";
    };
    package = lib.mkOption {
      description = "The icon theme package";
      type = lib.types.package;
      default = pkgs.numix-icon-theme-circle;
    };
    recolor = {
      enable = lib.mkOption {
        description = "Whether to recolor the icon theme colors.";
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
      saturation = lib.mkOption {
        description = ''Override icon saturation with custom value. Values between 0.0 and 1.0.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      saturationMultiply = lib.mkOption {
        description = ''Multiply icon saturation by value.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      light = lib.mkOption {
        description = ''Override icon light with custom value. Values between 0.0 and 1.0.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      lightMultiply = lib.mkOption {
        description = ''Multiply icon light by value.'';
        type = (lib.types.nullOr lib.types.str);
        default = null;
      };
      overrideWhite = lib.mkOption {
        description = ''Only used when light/lightMultiply is set.
          If set to true, the light value of white colors will get overriden
          causing the icon to lose the contrast between the different colors.
          recommended to keep as false/null.
        '';
        type = lib.types.bool;
        default = false;
      };
      dontOverrideWhiteThreshold = lib.mkOption {
        description = ''Only used when overrideWhite is false and light/lightMultiply is set.
          This speicifes the distance which anything bigger than will not considered white
          and it's light value will get overriden by light/lightMultiply.
        '';
        type = lib.types.str;
        default = "35";
      };
      smooth = lib.mkOption {
        description = "#TODO explain this? kinda hard, showing some pictures will be easier.";
        type = lib.types.bool;
        default = true;
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

    config = cfg.enable {
      home-manager.users.${config.modules.user.name} = {
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
                  postInstall = with cfg.recolor; (oldAttrs.postInstall or "") + ''
                    ${pythonEnv}/bin/python ${./recolor.py} --src $out/share/icons \
                    ${if isNull saturation then "" else "--saturation ${saturation}"} \
                    ${if isNull saturationMultiply then "" else "--saturation-multiply ${saturationMultiply}"} \
                    ${if isNull light then "" else "--light ${light}"} \
                    ${if isNull lightMultiply then "" else "--light-multiply ${lightMultiply}"} \
                    --override-white '${toString overrideWhite}' \
                    --dont-override-white-threshold ${dontOverrideWhiteThreshold} \
                    --smooth '${toString smooth}' \
                    ${if cfg.recolor.mode == "monochrome" then
                      "--monochrome '${builtins.concatStringsSep "," cfg.recolor.colors}'"
                    else
                      "--palette ''${builtins.concatStringsSep "," cfg.recolor.colors}''"}

                    for theme in $out/share/icons/*; do
                      gtk-update-icon-cache $theme
                    done
                  '';
                });
            })
          ];
        };
      };
    };
  };
}
