{ pkgs, lib, config, ... }:

let
  cfg = config.stylix.targets.iconTheme;
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
          palette = uses the entire colorscheme for each icon
        '';
        type = lib.types.enum [ "color" "color-from-palette" "palette" ];
        default = "palette";
      };
      colors = lib.mkOption {
        description = "The color to use when mode is set to color";
        type = lib.types.listOf (lib.types.str);
        default =
          if cfg.adjustColorScheme.mode == "color" then
            config.lib.stylix.colors.withHashtag.base09
          else if cfg.adjustColorScheme.mode == "color-from-palette" then
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
          iconTheme = lib.mkMerge [
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
                  postInstall = (oldAttrs.postInstall or "") + ''
                    python3 ${./recolor.py} --src $out/share/icons --smooth '${toString cfg.adjustColorScheme.smooth}' \
                    ${if cfg.adjustColorScheme.mode == "color" then
                      "--color '${builtins.head cfg.adjustColorScheme.colors}'"
                    else if cfg.adjustColorScheme.mode == "color-from-palette" then
                      "--color-from-palette '${builtins.concatStringsSep "," cfg.adjustColorScheme.colors}'"
                    else
                      "--palette ''${builtins.concatStringsSep "," cfg.adjustColorScheme.colors}''"}

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
