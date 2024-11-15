{ config, lib, ... }:
with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;
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
  options.stylix.targets.waybar = {
    enable = config.lib.stylix.mkEnableTarget "Waybar" true;
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

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.waybar.enable)
      {
        programs.waybar.style =
          ''
            @define-color base00 ${base00}; @define-color base01 ${base01}; @define-color base02 ${base02}; @define-color base03 ${base03};
            @define-color base04 ${base04}; @define-color base05 ${base05}; @define-color base06 ${base06}; @define-color base07 ${base07};

            @define-color base08 ${base08}; @define-color base09 ${base09}; @define-color base0A ${base0A}; @define-color base0B ${base0B};
            @define-color base0C ${base0C}; @define-color base0D ${base0D}; @define-color base0E ${base0E}; @define-color base0F ${base0F};

            * {
                font-family: "${sansSerif.name}";
                font-size: ${builtins.toString sizes.desktop}pt;
            }

            window#waybar, tooltip {
                background: alpha(@base00, ${
                  with config.stylix.opacity; builtins.toString desktop
                });
                color: @base05;
            }

            tooltip {
                border-color: @base0D;
            }
          ''
          + (builtins.readFile ./base.css)
          + (
            if config.stylix.targets.waybar.enableLeftBackColors then
              (import ./colors.nix "left")
            else
              colorlessModules "left"
          )
          + (
            if config.stylix.targets.waybar.enableCenterBackColors then
              (import ./colors.nix "center")
            else
              colorlessModules "center"
          )
          + (
            if config.stylix.targets.waybar.enableRightBackColors then
              (import ./colors.nix "right")
            else
              colorlessModules "right"
          );
      };
}
