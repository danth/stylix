{ pkgs, lib, config, ... }:

let
  cfg = config.stylix.targets.iconTheme;
in
{
  options.stylix.targets.iconTheme = {
    enable = config.lib.stylix.mkEnableTarget "iconTheme" true;
    name = lib.mkOption {
      description = "The iconn theme name";
      type = lib.types.str;
      default = "Numix-Circle";
    };
    package = lib.mkOption {
      description = "The icon theme package";
      type = lib.types.package;
      default = pkgs.numix-icon-theme-circle;
    };
    adjustColorScheme = {
      enable = lib.mkOption {
        description = "Whether to adjust the icon theme colors.";
        type = lib.types.bool;
        default = true;
      };
      mode = lib.mkOption {
        description = ''The mode to use:
          color = single color
          color-from-palette = uses a random single color from the palette for each icon
          palette = uses the entire colorscheme for each icon"
        '';
        type = lib.types.enum [ "color" "color-from-palette" "palette" ];
        default = "palette";
      };
      colorModeColor = lib.mkOption {
        description = "The color to use when mode is set to color";
        type = lib.types.str;
        default = config.lib.stylix.colors.withHashtag.base09;
      };
      smooth = lib.mkOption {
        description = "#TODO explain this? kinda hard, showing some pictures will be easier.";
        type = lib.types.bool;
        default = true;
      };
    };
  };

  config = cfg.enable {
    home-manager.users.${config.modules.user.name} = {
      gtk = {
        enable = true;
        iconTheme = mkMerge [
          {
            name = cfg.name;
          }

          (lib.mkIf (!cfg.adjustColorScheme.enable) {
            package = cfg.package;
          })
          (lib.mkIf (cfg.adjustColorScheme.enable) {
            package = cfg.package.overrideAttrs
              (oldAttrs: rec {
                propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [
                  (pkgs.python3.withPackages
                    (ps: with ps; [
                      colormath
                      tqdm
                      pillow
                    ])
                  )
                ];
                postInstall = with cfg.adjustColorScheme; with config.lib.stylix.colors.withHashtag; ''
                  python3 ${./recolor.py} --src $out/share/icons --smooth '${toString smooth}' \
                  ${if cfg.adjustColorScheme.mode == "color" then "--color '${colorModeColor}'"
                  else if cfg.adjustColorScheme.mode == "color-from-palette" then "--color-from-palette '${base08},${base09},${base0A},${base0B},${base0C},${base0D},${base0E},${base0F}'"
                  else "--palette '${base00},${base01},${base02},${base03},${base04},${base05},${base06},${base07},${base08},${base09},${base0A},${base0B},${base0C},${base0D},${base0E},${base0F}'"}
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
}
