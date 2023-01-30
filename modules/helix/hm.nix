{ config, lib, ... }:

with config.lib.stylix.colors.withHashtag;

# Based upon https://github.com/helix-editor/helix/blob/f8c83f98859fd618980141eb95e7927dcdf074d7/runtime/themes/base16_default_dark.toml
let theme = {
  "ui.background".bg = base00;
  "ui.menu" = {
    fg = base05;
    bg = base01;
  };
  "ui.menu.selected" = {
    fg = base01;
    bg = base04;
  };
  "ui.linenr" = {
    fg = base03;
    bg = base01;
  };
  "ui.popup".bg = base01;
  "ui.window".bg = base01;
  "ui.linenr.selected" = {
    fg = base04;
    bg = base01;
    modifiers = [ "bold" ];
  };
  "ui.selection".bg = base02;
  "comment" = {
    fg = base03;
    modifiers = [ "italic" ];
  };
  "ui.statusline" = {
    fg = base04;
    bg = base01;
  };
  "ui.cursor" = {
    fg = base04;
    modifiers = [ "reversed" ];
  };
  "ui.cursor.primary" = {
    fg = base05;
    modifiers = [ "reversed" ];
  };
  "ui.text" = base05;
  "operator" = base05;
  "ui.text.focus" = base05;
  "variable" = base08;
  "constant.numeric" = base09;
  "constant" = base09;
  "attributes" = base09;
  "type" = base0A;
  "ui.cursor.match" = {
    fg = base0A;
    modifiers = [ "underlined" ];
  };
  "string"  = base0B;
  "variable.other.member" = base0B;
  "constant.character.escape" = base0C;
  "function" = base0D;
  "constructor" = base0D;
  "special" = base0D;
  "keyword" = base0E;
  "label" = base0E;
  "namespace" = base0E;
  "ui.help" = {
    fg = base06;
    bg = base01;
  };

  "markup.heading" = base0D;
  "markup.list" = base08;
  "markup.bold" = {
    fg = base0A;
    modifiers = [ "bold" ];
  };
  "markup.italic" = {
    fg = base0E;
    modifiers = [ "italic" ];
  };
  "markup.link.url" = {
    fg = base09;
    modifiers = [ "underlined" ];
  };
  "markup.link.text" = base08;
  "markup.quote" = base0C;
  "markup.raw" = base0B;

  "diff.plus" = base0B;
  "diff.delta" = base09;
  "diff.minus" = base08;

  "diagnostic".modifiers = [ "underlined" ];
  "ui.gutter".bg = base01;
  "info" = base0D;
  "hint" = base03;
  "debug" = base03;
  "warning" = base09;
  "error" = base08;
};

in {
  options.stylix.targets.helix.enable =
    config.lib.stylix.mkEnableTarget "Helix" true;

  config = lib.mkIf config.stylix.targets.helix.enable {
    programs.helix = {
      settings.theme = "stylix";
      themes.stylix = theme;
    };
  };
}
