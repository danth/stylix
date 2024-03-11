# Based on the official catppuccin themes https://github.com/yazi-rs/themes
{
  config,
  lib,
  ...
}: {
  options.stylix.targets.yazi = {
    enable = config.lib.stylix.mkEnableTarget "Yazi" config.programs.yazi.enable;
  };

  config = lib.mkIf config.stylix.targets.yazi.enable {
    programs.yazi.theme = with config.lib.stylix.colors.withHashtag; let
      mkFg = fg: {inherit fg;};
      mkBg = bg: {inherit bg;};
      mkBoth = fg: bg: {inherit fg bg;};
      mkSame = c: (mkBoth c c);
    in {
      manager = rec {
        # Reusing bat themes, since it's suggested in the stying guide
        # https://yazi-rs.github.io/docs/configuration/theme#manager
        syntect_theme = config.lib.stylix.colors {
          template = ../bat/base16-stylix.mustache;
          extension = ".tmTheme";
        };

        cwd = mkFg base0C;
        hovered = (mkBoth base05 base03) // {bold = true;};
        preview_hovered = hovered;
        find_keyword = (mkFg base0B) // {bold = true;};
        find_position = mkFg base05;
        marker_selected = mkSame base0B;
        marker_copied = mkSame base0A;
        marker_cut = mkSame base08;
        tab_active = mkBoth base00 base0D;
        tab_inactive = mkBoth base05 base01;
        border_style = mkFg base04;
      };

      status = {
        separator_style = mkSame base01;
        mode_normal = (mkBoth base00 base0D) // {bold = true;};
        mode_select = (mkBoth base00 base0B) // {bold = true;};
        mode_unset = (mkBoth base00 base0F) // {bold = true;};
        progress_label = mkBoth base05 base00;
        progress_normal = mkBoth base05 base00;
        progress_error = mkBoth base08 base00;
        permissions_t = mkFg base0D;
        permissions_r = mkFg base0A;
        permissions_w = mkFg base08;
        permissions_x = mkFg base0B;
        permissions_s = mkFg base0C;
      };

      select = {
        border = mkFg base0D;
        active = mkFg base0E;
        inactive = mkFg base05;
      };

      input = {
        border = mkFg base0D;
        title = mkFg base05;
        value = mkFg base05;
        selected = mkBg base03;
      };

      completion = {
        border = mkFg base0D;
        active = mkBoth base0E base03;
        inactive = mkFg base05;
      };

      tasks = {
        border = mkFg base0D;
        title = mkFg base05;
        hovered = mkBoth base05 base03;
      };

      which = {
        mask = mkBg base02;
        cand = mkFg base0C;
        rest = mkFg base0F;
        desc = mkFg base05;
        separator_style = mkFg base04;
      };

      help = {
        on = mkFg base0E;
        run = mkFg base0C;
        desc = mkFg base05;
        hovered = mkBoth base05 base03;
        footer = mkFg base05;
      };

      # https://github.com/sxyazi/yazi/blob/main/yazi-config/preset/theme.toml
      filetype.rules = let
        mkRule = mime: fg: {inherit mime fg;};
      in [
        (mkRule "image/*" base0C)
        (mkRule "video/*" base0A)
        (mkRule "audio/*" base0A)

        (mkRule "application/zip" base0E)
        (mkRule "application/gzip" base0E)
        (mkRule "application/x-tar" base0E)
        (mkRule "application/x-bzip" base0E)
        (mkRule "application/x-bzip2" base0E)
        (mkRule "application/x-7z-compressed" base0E)
        (mkRule "application/x-rar" base0E)
        (mkRule "application/xz" base0E)

        (mkRule "application/doc" base0B)
        (mkRule "application/pdf" base0B)
        (mkRule "application/rtf" base0B)
        (mkRule "application/vnd.*" base0B)

        ((mkRule "inode/directory" base0D) // {bold = true;})
        (mkRule "*" base05)
      ];
    };
  };
}
