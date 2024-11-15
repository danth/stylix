# Based on the official catppuccin themes https://github.com/yazi-rs/themes
{
  config,
  lib,
  ...
}:
{
  options.stylix.targets.yazi = {
    enable = config.lib.stylix.mkEnableTarget "Yazi" true;
  };

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.yazi.enable) {
    programs.yazi.theme =
      with config.lib.stylix.colors.withHashtag;
      let
        mkFg = fg: { inherit fg; };
        mkBg = bg: { inherit bg; };
        mkBoth = fg: bg: { inherit fg bg; };
        mkSame = c: (mkBoth c c);
      in
      {
        manager = rec {
          # Reusing bat themes, since it's suggested in the stying guide
          # https://yazi-rs.github.io/docs/configuration/theme#manager
          syntect_theme = config.lib.stylix.colors {
            template = ../bat/base16-stylix.mustache;
            extension = ".tmTheme";
          };

          cwd = mkFg cyan;
          hovered = (mkBoth base05 base03) // {
            bold = true;
          };
          preview_hovered = hovered;
          find_keyword = (mkFg green) // {
            bold = true;
          };
          find_position = mkFg base05;
          marker_selected = mkSame yellow;
          marker_copied = mkSame green;
          marker_cut = mkSame red;
          tab_active = mkBoth base00 blue;
          tab_inactive = mkBoth base05 base01;
          border_style = mkFg base04;
        };

        mode = {
          normal_main = (mkBoth base00 blue) // {
            bold = true;
          };
          normal_alt = mkBoth blue base00;
          select_main = (mkBoth base00 green) // {
            bold = true;
          };
          select_alt = mkBoth green base00;
          unset_main = (mkBoth base00 brown) // {
            bold = true;
          };
          unset_alt = mkBoth brown base00;
        };

        status = {
          progress_label = mkBoth base05 base00;
          progress_normal = mkBoth base05 base00;
          progress_error = mkBoth red base00;
          perm_type = mkFg blue;
          perm_read = mkFg yellow;
          perm_write = mkFg red;
          perm_exec = mkFg green;
          perm_sep = mkFg cyan;
        };

        pick = {
          border = mkFg blue;
          active = mkFg magenta;
          inactive = mkFg base05;
        };

        input = {
          border = mkFg blue;
          title = mkFg base05;
          value = mkFg base05;
          selected = mkBg base03;
        };

        completion = {
          border = mkFg blue;
          active = mkBoth magenta base03;
          inactive = mkFg base05;
        };

        tasks = {
          border = mkFg blue;
          title = mkFg base05;
          hovered = mkBoth base05 base03;
        };

        which = {
          mask = mkBg base02;
          cand = mkFg cyan;
          rest = mkFg brown;
          desc = mkFg base05;
          separator_style = mkFg base04;
        };

        help = {
          on = mkFg magenta;
          run = mkFg cyan;
          desc = mkFg base05;
          hovered = mkBoth base05 base03;
          footer = mkFg base05;
        };

        # https://github.com/sxyazi/yazi/blob/main/yazi-config/preset/theme.toml
        filetype.rules =
          let
            mkRule = mime: fg: { inherit mime fg; };
          in
          [
            (mkRule "image/*" cyan)
            (mkRule "video/*" yellow)
            (mkRule "audio/*" yellow)

            (mkRule "application/zip" magenta)
            (mkRule "application/gzip" magenta)
            (mkRule "application/tar" magenta)
            (mkRule "application/bzip" magenta)
            (mkRule "application/bzip2" magenta)
            (mkRule "application/7z-compressed" magenta)
            (mkRule "application/rar" magenta)
            (mkRule "application/xz" magenta)

            (mkRule "application/doc" green)
            (mkRule "application/pdf" green)
            (mkRule "application/rtf" green)
            (mkRule "application/vnd.*" green)

            ((mkRule "inode/directory" blue) // { bold = true; })
            (mkRule "*" base05)
          ];
      };
  };
}
