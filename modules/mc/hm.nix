{ config, lib, pkgs, ... }:

with lib;

let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  config = mkIf (builtins.elem pkgs.mc config.home.packages) {
    home.file.".config/mc/ini".text = ''
      [Midnight-Commander]
      skin=stylix
    '';

    home.file.".local/share/mc/skins/stylix.ini".text = ''
      [skin]
      description = Stylix Theme Matching lsd Output
      truecolors = true

      [Lines]
      horiz = ─
      vert = │
      lefttop = ┌
      righttop = ┐
      leftbottom = └
      rightbottom = ┘
      topmiddle = ┬
      bottommiddle = ┴
      leftmiddle = ├
      rightmiddle = ┤
      cross = ┼
      dhoriz = ─
      dvert = │
      dlefttop = ┌
      drighttop = ┐
      dleftbottom = └
      drightbottom = ┘
      dtopmiddle = ┬
      dbottommiddle = ┴
      dleftmiddle = ├
      drightmiddle = ┤

      [core]
      _default_ = ${colors.base05};${colors.base00}
      selected = ${colors.base00};${colors.base0D}
      marked = ${colors.base0B};${colors.base00}
      markselect = ${colors.base0B};${colors.base0D}
      gauge = ${colors.base05};${colors.base00}
      input = ${colors.base05};${colors.base01}
      inputunchanged = ${colors.base04};${colors.base01}
      inputmark = ${colors.base00};${colors.base0D}
      disabled = ${colors.base03};${colors.base01}
      reverse = ${colors.base05};${colors.base00}
      commandlinemark = ${colors.base00};${colors.base0D}
      header = ${colors.base05};${colors.base00}

      [dialog]
      _default_ = ${colors.base05};${colors.base00}
      dfocus = ${colors.base00};${colors.base0D}
      dhotnormal = ${colors.base0B};${colors.base00}
      dhotfocus = ${colors.base0B};${colors.base0D}
      dtitle = ${colors.base0A};${colors.base00}

      [error]
      _default_ = ${colors.base08};${colors.base00}
      errdfocus = ${colors.base00};${colors.base08}
      errdhotnormal = ${colors.base0B};${colors.base00}
      errdhotfocus = ${colors.base0B};${colors.base08}
      errdtitle = ${colors.base0A};${colors.base00}

      [filehighlight]
      directory = ${colors.base0D};
      executable = ${colors.base0B};
      symlink = ${colors.base0C};
      hardlink = ${colors.base06};
      stalelink = ${colors.base08};
      device = ${colors.base0B};
      special = ${colors.base0E};
      core = ${colors.base08};
      temp = ${colors.base0C};
      archive = ${colors.base0A};
      doc = ${colors.base05};
      source = ${colors.base0D};
      media = ${colors.base0B};
      graph = ${colors.base0C};
      database = ${colors.base09};

      [menu]
      _default_ = ${colors.base05};${colors.base00}
      menusel = ${colors.base00};${colors.base0D}
      menuhot = ${colors.base0B};${colors.base00}
      menuhotsel = ${colors.base0B};${colors.base0D}
      menuinactive = ${colors.base03};${colors.base00}

      [popupmenu]
      _default_ = ${colors.base05};${colors.base00}
      menusel = ${colors.base00};${colors.base0D}
      menutitle = ${colors.base05};${colors.base00}

      [buttonbar]
      hotkey = ${colors.base00};${colors.base0D}
      button = ${colors.base05};${colors.base00}

      [statusbar]
      _default_ = ${colors.base05};${colors.base00}

      [help]
      _default_ = ${colors.base05};${colors.base00}
      helpitalic = ${colors.base08};${colors.base00}
      helpbold = ${colors.base0A};${colors.base00}
      helplink = ${colors.base0D};${colors.base00}
      helpslink = ${colors.base00};${colors.base0D}
      helptitle = ${colors.base0A};${colors.base00}

      [editor]
      _default_ = ${colors.base05};${colors.base00}
      editbold = ${colors.base0A};${colors.base0D}
      editmarked = ${colors.base00};${colors.base0C}
      editwhitespace = ${colors.base0D};${colors.base0D}
      editlinestate = ${colors.base05};${colors.base0C}
      bookmark = ${colors.base05};${colors.base08}
      bookmarkfound = ${colors.base00};${colors.base0B}
      editrightmargin = ${colors.base0D};${colors.base00}
      editframe = ${colors.base0A};
      editframeactive = ${colors.base05};
      editframedrag = ${colors.base0D};

      [viewer]
      _default_ = ${colors.base05};${colors.base00}
      viewbold = ${colors.base0A};${colors.base0D}
      viewunderline = ${colors.base08};${colors.base0D}
      viewselected = ${colors.base0A};${colors.base0C}

      [diffviewer]
      added = ${colors.base05};${colors.base0B}
      changedline = ${colors.base0D};${colors.base0C}
      changednew = ${colors.base08};${colors.base0C}
      changed = ${colors.base05};${colors.base0C}
      removed = ${colors.base05};${colors.base08}
      error = ${colors.base08};${colors.base05}

      [widget-common]
      sort-sign-up = ↑
      sort-sign-down = ↓

      [widget-panel]
      hiddenfiles-sign-show = •
      hiddenfiles-sign-hide = ○
      history-prev-item-sign = «
      history-next-item-sign = »
      history-show-list-sign = ^
      filename-scroll-left-char = «
      filename-scroll-right-char = »

      [widget-editor]
      window-state-char = ↕
      window-close-char = ✕
    '';
  };
}
