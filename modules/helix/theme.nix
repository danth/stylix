# Converted to Nix from https://github.com/tinted-theming/base16-helix/blob/03860521c40b0b9c04818f2218d9cc9efc21e7a5/templates/default.mustache
# Copyright (c) 2022 Karsten Gebbert <k@ioctl.it>
# Copyright (c) 2022 [Tinted Theming](https://github.com/tinted-theming)
config:
let
  inherit (config.lib.stylix) colors;
  bg = if config.stylix.opacity.terminal == 1.0 then "base00" else "";
in
{
  constant = "base09";
  "constant.character.escape" = "base0C";
  "constant.numeric" = "base09";
  constructor = "base0D";
  debug = "base03";
  "diff.delta" = "base09";
  "diff.minus" = "base08";
  "diff.plus" = "base0B";
  error = "base08";
  function = "base0D";
  hint = "base03";
  info = "base0D";
  keyword = "base0E";
  label = "base0E";
  namespace = "base0E";
  operator = "base05";
  special = "base0D";
  string = "base0B";
  type = "base0A";
  variable = "base08";
  "variable.other.member" = "base0B";
  warning = "base09";

  comment = {
    fg = "base03";
    modifiers = [ "italic" ];
  };

  diagnostic = {
    modifiers = [ "underlined" ];
  };

  "markup.bold" = {
    fg = "base0A";
    modifiers = [ "bold" ];
  };
  "markup.heading.1" = {
    fg = "base0D";
    modifiers = [ "bold" ];
  };
  "markup.heading.2" = {
    fg = "base08";
    modifiers = [ "bold" ];
  };
  "markup.heading.3" = {
    fg = "base09";
    modifiers = [ "bold" ];
  };
  "markup.heading.4" = {
    fg = "base0A";
    modifiers = [ "bold" ];
  };
  "markup.heading.5" = {
    fg = "base0B";
    modifiers = [ "bold" ];
  };
  "markup.heading.6" = {
    fg = "base0C";
    modifiers = [ "bold" ];
  };
  "markup.italic" = {
    fg = "base0E";
    modifiers = [ "italic" ];
  };
  "markup.link.text" = "base08";
  "markup.link.url" = {
    fg = "base09";
    modifiers = [ "underlined" ];
  };
  "markup.list" = "base08";
  "markup.quote" = "base0C";
  "markup.raw" = "base0B";
  "markup.strikethrough" = {
    modifiers = [ "crossed_out" ];
  };

  "diagnostic.hint".underline.style = "curl";
  "diagnostic.info".underline.style = "curl";
  "diagnostic.warning".underline.style = "curl";
  "diagnostic.error".underline.style = "curl";

  "ui.background".bg = bg;
  "ui.bufferline.active" = {
    fg = bg;
    bg = "base03";
    modifiers = [ "bold" ];
  };
  "ui.bufferline" = {
    fg = "base04";
    inherit bg;
  };
  "ui.cursor" = {
    fg = "base05";
    modifiers = [ "reversed" ];
  };
  "ui.cursor.insert" = {
    fg = "base05";
    modifiers = [ "reversed" ];
  };
  "ui.cursorline.primary" = {
    fg = "base05";
    bg = "base01";
  };
  "ui.cursor.match" = {
    fg = "base05";
    bg = "base02";
    modifiers = [ "bold" ];
  };
  "ui.cursor.select" = {
    fg = "base05";
    modifiers = [ "reversed" ];
  };
  "ui.gutter" = {
    inherit bg;
  };
  "ui.help" = {
    fg = "base06";
    bg = "base01";
  };
  "ui.linenr" = {
    fg = "base03";
    inherit bg;
  };
  "ui.linenr.selected" = {
    fg = "base04";
    bg = "base01";
    modifiers = [ "bold" ];
  };
  "ui.menu" = {
    fg = "base05";
    bg = "base01";
  };
  "ui.menu.scroll" = {
    fg = "base03";
    bg = "base01";
  };
  "ui.menu.selected" = {
    fg = "base01";
    bg = "base04";
  };
  "ui.popup".bg = "base01";
  "ui.selection".bg = "base02";
  "ui.selection.primary".bg = "base02";
  "ui.statusline" = {
    fg = "base04";
    bg = "base01";
  };
  "ui.statusline.inactive" = {
    bg = "base01";
    fg = "base03";
  };
  "ui.statusline.insert" = {
    fg = bg;
    bg = "base0B";
  };
  "ui.statusline.normal" = {
    fg = bg;
    bg = "base03";
  };
  "ui.statusline.select" = {
    fg = bg;
    bg = "base0F";
  };
  "ui.text" = "base05";
  "ui.text.directory" = "base0D";
  "ui.text.focus" = "base05";
  "ui.virtual.indent-guide".fg = "base03";
  "ui.virtual.inlay-hint".fg = "base03";
  "ui.virtual.ruler".bg = "base01";
  "ui.virtual.jump-label" = {
    fg = "base0A";
    modifiers = [ "bold" ];
  };
  "ui.window".bg = "base01";

  palette = {
    inherit (colors.withHashtag)
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
      ;
  };
}
