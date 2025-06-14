{ mkTarget, lib, ... }:
mkTarget {
  name = "waybar";
  humanName = "Waybar";

  extraOptions = {
    background = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      description = "Used to set bg even if `opacity` or `colors` is null";
      internal = true;
      default = null;
    };
    font = lib.mkOption {
      type = lib.types.enum [
        "serif"
        "sansSerif"
        "monospace"
        "emoji"
      ];
      default = "monospace";
      example = "sansSerif";
      description = "The font for waybar to use";
    };
    addCss = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "adds fully functional css (otherwise just adds colors and fonts)";
    };
    enableLeftBackColors = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "enables background colors on the left side of the bar";
    };
    enableCenterBackColors = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "enables background colors on the center of the bar";
    };
    enableRightBackColors = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "enables background colors on the right side of the bar";
    };
  };

  configElements = [
    (
      { cfg }:
      {
        programs.waybar.style = lib.mkIf (cfg.addCss && cfg.background != null) ''
          window#waybar, tooltip {
              background: ${cfg.background};
          }
        '';
      }
    )
    (
      {
        cfg,
        opacity,
        _colors,
      }:
      {
        stylix.targets.waybar.background = lib.mkIf cfg.addCss "alpha(@base00, ${builtins.toString opacity.desktop})";
      }
    )
    (
      { cfg, fonts }:
      {
        programs.waybar.style = ''
          * {
              font-family: "${fonts.${cfg.font}.name}";
              font-size: ${builtins.toString fonts.sizes.desktop}pt;
          }
        '';
      }
    )
    (
      {
        cfg,
        colors,
      }:
      let
        colorlessModules = place: ''
          .modules-${place} #workspaces button {
              border-bottom: 3px solid transparent;
          }
          .modules-${place} #workspaces button.focused,
          .modules-${place} #workspaces button.active {
              border-bottom: 3px solid @base05;
          }
        '';
      in
      {
        stylix.targets.waybar.background = lib.default "@base00";
        programs.waybar.style =
          with colors.withHashtag;
          ''
            @define-color base00 ${base00}; @define-color base01 ${base01};
            @define-color base02 ${base02}; @define-color base03 ${base03};
            @define-color base04 ${base04}; @define-color base05 ${base05};
            @define-color base06 ${base06}; @define-color base07 ${base07};

            @define-color base08 ${base08}; @define-color base09 ${base09};
            @define-color base0A ${base0A}; @define-color base0B ${base0B};
            @define-color base0C ${base0C}; @define-color base0D ${base0D};
            @define-color base0E ${base0E}; @define-color base0F ${base0F};
          ''
          + lib.optionalString cfg.addCss (
            ''
              window#waybar, tooltip {
                  color: @base05;
              }

              tooltip {
                  border-color: @base0D;
              }
            ''
            + (builtins.readFile ./base.css)
            + (
              if cfg.enableLeftBackColors then
                (import ./colors.nix "left")
              else
                colorlessModules "left"
            )
            + (
              if cfg.enableCenterBackColors then
                (import ./colors.nix "center")
              else
                colorlessModules "center"
            )
            + (
              if cfg.enableRightBackColors then
                (import ./colors.nix "right")
              else
                colorlessModules "right"
            )
          );
      }
    )
  ];
}
