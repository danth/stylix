{ config, lib, ... }:

let colors = config.lib.stylix.colors.withHashtag;
in {
  options.stylix.targets.wezterm.enable =
    config.lib.stylix.mkEnableTarget "wezterm" config.programs.wezterm.enable;

  config = lib.mkIf config.stylix.targets.wezterm.enable {

    programs.wezterm.colorSchemes.stylix = with colors; {
      ansi = [ base00 base08 base0A base0D base0E base0C base0C base05 ];
      brights = [ base03 base08 base0B base0A base0D base0E base0C base07 ];
      background = base00;
      cursor_bg = base05;
      cursor_fg = base05;
      compose_cursor = base06;
      foreground = base05;
      scrollbar_thumb = base01;
      selection_bg = base05;
      selection_fg = base00;
      split = base03;
      visual_bell = base09;
      tab_bar = {
        background = base01;
        inactive_tab_edge = base01;
        active_tab = {
          bg_color = base03;
          fg_color = base05;
        };
        inactive_tab = {
          bg_color = base00;
          fg_color = base05;
        };
        inactive_tab_hover = {
          bg_color = base05;
          fg_color = base00;
        };
        new_tab = {
          bg_color = base00;
          fg_color = base05;
        };
        new_tab_hover = {
          bg_color = base05;
          fg_color = base00;
        };
      };
    };

    xdg.configFile."wezterm/wezterm.lua".text = with colors;
      with config.stylix.fonts;
      lib.mkForce ''
        -- Generated by Stylix
        local wezterm = require("wezterm")
        wezterm.add_to_config_reload_watch_list(wezterm.config_dir)
        local function stylix_wrapped_config()
            ${config.programs.wezterm.extraConfig}
        end
        local stylix_base_config = wezterm.config_builder()
        local stylix_user_config = stylix_wrapped_config()
        stylix_base_config = {
            -- Set due to the default fancy tabs not respecting colorschemes
            -- See https://github.com/wez/wezterm/issues/2615
            use_fancy_tab_bar = false,
            color_scheme = "stylix",
            font = wezterm.font_with_fallback {
                "${monospace.name}",
                "${emoji.name}",
            },
            font_size = ${builtins.toString sizes.terminal},
            window_background_opacity = ${
              builtins.toString config.stylix.opacity.terminal
            },
            window_frame = {
                active_titlebar_bg = "${base03}",
                active_titlebar_fg = "${base05}",
                active_titlebar_border_bottom = "${base03}",
                border_left_color = "${base01}",
                border_right_color = "${base01}",
                border_bottom_color = "${base01}",
                border_top_color = "${base01}",
                button_bg = "${base01}",
                button_fg = "${base05}",
                button_hover_bg = "${base05}",
                button_hover_fg = "${base03}",
                inactive_titlebar_bg = "${base01}",
                inactive_titlebar_fg = "${base05}",
                inactive_titlebar_border_bottom = "${base03}",
            },
            command_palette_bg_color = "${base01}",
            command_palette_fg_color = "${base05}",
            command_palette_font_size = ${builtins.toString sizes.popups},
        }
        for key, value in pairs(stylix_user_config) do
            stylix_base_config[key] = value
        end
        return stylix_base_config
      '';
  };
}
