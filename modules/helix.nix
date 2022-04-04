{ config, ... }:

with config.lib.stylix.colors;

# Based upon https://github.com/helix-editor/helix/blob/f8c83f98859fd618980141eb95e7927dcdf074d7/runtime/themes/base16_default_dark.toml
let theme = {
  "ui.background".bg = base00-hash;
  "ui.menu" = {
    fg = base05-hash;
    bg = base01-hash;
  };
  "ui.menu.selected" = {
    fg = base01-hash;
    bg = base04-hash;
  };
  "ui.linenr" = {
    fg = base03-hash;
    bg = base01-hash;
  };
  "ui.popup".bg = base01-hash;
  "ui.window".bg = base01-hash;
  "ui.linenr.selected" = {
    fg = base04-hash;
    bg = base01-hash;
    modifiers = [ "bold" ];
  };
  "ui.selection".bg = base02-hash;
  "comment" = {
    fg = base03-hash;
    modifiers = [ "italic" ];
  };
  "ui.statusline" = {
    fg = base04-hash;
    bg = base01-hash;
  };
  "ui.cursor" = {
    fg = base04-hash;
    modifiers = [ "reversed" ];
  };
  "ui.cursor.primary" = {
    fg = base05-hash;
    modifiers = [ "reversed" ];
  };
  "ui.text" = base05-hash;
  "operator" = base05-hash;
  "ui.text.focus" = base05-hash;
  "variable" = base08-hash;
  "constant.numeric" = base09-hash;
  "constant" = base09-hash;
  "attributes" = base09-hash;
  "type" = base0A-hash;
  "ui.cursor.match" = {
    fg = base0A-hash;
    modifiers = [ "underlined" ];
  };
  "string"  = base0B-hash;
  "variable.other.member" = base0B-hash;
  "constant.character.escape" = base0C-hash;
  "function" = base0D-hash;
  "constructor" = base0D-hash;
  "special" = base0D-hash;
  "keyword" = base0E-hash;
  "label" = base0E-hash;
  "namespace" = base0E-hash;
  "ui.help" = {
    fg = base06-hash;
    bg = base01-hash;
  };

  "markup.heading" = base0D-hash;
  "markup.list" = base08-hash;
  "markup.bold" = {
    fg = base0A-hash;
    modifiers = [ "bold" ];
  };
  "markup.italic" = {
    fg = base0E-hash;
    modifiers = [ "italic" ];
  };
  "markup.link.url" = {
    fg = base09-hash;
    modifiers = [ "underlined" ];
  };
  "markup.link.text" = base08-hash;
  "markup.quote" = base0C-hash;
  "markup.raw" = base0B-hash;

  "diff.plus" = base0B-hash;
  "diff.delta" = base09-hash;
  "diff.minus" = base08-hash;

  "diagnostic".modifiers = [ "underlined" ];
  "ui.gutter".bg = base01-hash;
  "info" = base0D-hash;
  "hint" = base03-hash;
  "debug" = base03-hash;
  "warning" = base09-hash;
  "error" = base08-hash;
};

in {
  stylix.homeModule = {
    programs.helix = {
      settings.theme = "stylix";
      themes.stylix = theme;
    };
  };
}
