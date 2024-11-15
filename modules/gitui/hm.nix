{ config, lib, ... }:
let
  inherit (config.lib.stylix) colors;

  mkRgb =
    color:
    let
      r = colors."${color}-rgb-r";
      g = colors."${color}-rgb-g";
      b = colors."${color}-rgb-b";
    in
    "Rgb(${r}, ${g}, ${b})";
in
{
  options.stylix.targets.gitui.enable =
    config.lib.stylix.mkEnableTarget "GitUI" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.gitui.enable) {
    programs.gitui.theme = ''
      (
          selected_tab: Some(Reset),
          command_fg: Some(${mkRgb "base05"}),
          selection_bg: Some(${mkRgb "base04"}),
          selection_fg: Some(${mkRgb "base05"}),
          cmdbar_bg: Some(${mkRgb "base01"}),
          cmdbar_extra_lines_bg: Some(${mkRgb "base01"}),
          disabled_fg: Some(${mkRgb "base04"}),
          diff_line_add: Some(${mkRgb "base0B"}),
          diff_line_delete: Some(${mkRgb "base08"}),
          diff_file_added: Some(${mkRgb "base0A"}),
          diff_file_removed: Some(${mkRgb "base08"}),
          diff_file_moved: Some(${mkRgb "base0E"}),
          diff_file_modified: Some(${mkRgb "base09"}),
          commit_hash: Some(${mkRgb "base07"}),
          commit_time: Some(${mkRgb "base05"}),
          commit_author: Some(${mkRgb "base0D"}),
          danger_fg: Some(${mkRgb "base08"}),
          push_gauge_bg: Some(${mkRgb "base0D"}),
          push_gauge_fg: Some(${mkRgb "base00"}),
          tag_fg: Some(${mkRgb "base06"}),
          branch_fg: Some(${mkRgb "base0C"})
      )
    '';
  };
}
