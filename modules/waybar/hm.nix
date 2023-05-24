{ config, lib, ... }:
with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;

{
  options.stylix.targets.waybar = {
      enable = config.lib.stylix.mkEnableTarget "Waybar" true;
      enableLeftBackColors = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = lib.mdDoc "enables background colors on the left side of the bar";
      };
      enableCenterBackColors = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = lib.mdDoc "enables background colors on the center of the bar";
      };
      enableRightBackColors = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = lib.mdDoc "enables background colors on the right side of the bar";
      };
  };

  config = lib.mkIf config.stylix.targets.waybar.enable {
    programs.waybar.style = ''
    @define-color base00 ${base00}; @define-color base01 ${base01}; @define-color base02 ${base02}; @define-color base03 ${base03};
    @define-color base04 ${base04}; @define-color base05 ${base05}; @define-color base06 ${base06}; @define-color base07 ${base07};

    @define-color base08 ${base08}; @define-color base09 ${base09}; @define-color base0A ${base0A}; @define-color base0B ${base0B};
    @define-color base0C ${base0C}; @define-color base0D ${base0D}; @define-color base0E ${base0E}; @define-color base0F ${base0F};
    * {
        border: none;
        border-radius: 0;
        font-family: ${sansSerif.name};
        font-size: ${builtins.toString sizes.desktop};
    }

    window#waybar {
        background: alpha(@base00, ${with config.stylix.opacity; builtins.toString desktop});
        color: @base05;
    }
    ''
    + (builtins.readFile ./base.css)
    + (if config.stylix.targets.waybar.enableLeftBackColors then builtins.readFile ./left.css else ''
    .modules-left #workspaces button {
        border-bottom: 3px solid transparent;
    }
    .modules-left #workspaces button.focused,
    .modules-left #workspaces button.active {
        border-bottom: 3px solid @base05;
    }
    '')
    + (if config.stylix.targets.waybar.enableCenterBackColors then builtins.readFile ./center.css else ''
    .modules-center #workspaces button {
        border-bottom: 3px solid transparent;
    }
    .modules-center #workspaces button.focused,
    .modules-center #workspaces button.active {
        border-bottom: 3px solid @base05;
    }
    '')
    + (if config.stylix.targets.waybar.enableRightBackColors then builtins.readFile ./right.css else ''
    .modules-right #workspaces button {
        border-bottom: 3px solid transparent;
    }
    .modules-right #workspaces button.focused,
    .modules-right #workspaces button.active {
        border-bottom: 3px solid @base05;
    }
    '');
  };
}
