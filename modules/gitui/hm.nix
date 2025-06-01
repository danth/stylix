{ mkTarget, ... }:
mkTarget {
  name = "gitui";
  humanName = "GitUI";

  configElements =
    { colors }:
    {
      programs.gitui.theme = with colors.withHashtag; ''
        (
            selected_tab: Some("Reset"),
            command_fg: Some("${base05}"),
            selection_bg: Some("${base04}"),
            selection_fg: Some("${base05}"),
            cmdbar_bg: Some("${base01}"),
            cmdbar_extra_lines_bg: Some("${base01}"),
            disabled_fg: Some("${base04}"),
            diff_line_add: Some("${base0B}"),
            diff_line_delete: Some("${base08}"),
            diff_file_added: Some("${base0A}"),
            diff_file_removed: Some("${base08}"),
            diff_file_moved: Some("${base0E}"),
            diff_file_modified: Some("${base09}"),
            commit_hash: Some("${base07}"),
            commit_time: Some("${base05}"),
            commit_author: Some("${base0D}"),
            danger_fg: Some("${base08}"),
            push_gauge_bg: Some("${base0D}"),
            push_gauge_fg: Some("${base00}"),
            tag_fg: Some("${base06}"),
            branch_fg: Some("${base0C}")
        )
      '';
    };
}
