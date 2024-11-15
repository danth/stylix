{ config, lib, ... }:

with config.stylix.fonts;

let
  inherit (config.lib.formats.rasi) mkLiteral;
  mkRgba =
    opacity: color:
    let
      c = config.lib.stylix.colors;
      r = c."${color}-rgb-r";
      g = c."${color}-rgb-g";
      b = c."${color}-rgb-b";
    in
    mkLiteral "rgba ( ${r}, ${g}, ${b}, ${opacity} % )";
  mkRgb = mkRgba "100";
  rofiOpacity = builtins.toString (
    builtins.ceil (config.stylix.opacity.popups * 100)
  );
in
{
  options.stylix.targets.rofi.enable =
    config.lib.stylix.mkEnableTarget "Rofi" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.rofi.enable) {
    programs.rofi = {
      font = "${monospace.name} ${toString sizes.popups}";
      theme = {
        "*" = {
          background = mkRgba rofiOpacity "base00";
          lightbg = mkRgba rofiOpacity "base01";
          red = mkRgba rofiOpacity "base08";
          blue = mkRgba rofiOpacity "base0D";
          lightfg = mkRgba rofiOpacity "base06";
          foreground = mkRgba rofiOpacity "base05";

          background-color = mkRgb "base00";
          separatorcolor = mkLiteral "@foreground";
          border-color = mkLiteral "@foreground";
          selected-normal-foreground = mkLiteral "@lightbg";
          selected-normal-background = mkLiteral "@lightfg";
          selected-active-foreground = mkLiteral "@background";
          selected-active-background = mkLiteral "@blue";
          selected-urgent-foreground = mkLiteral "@background";
          selected-urgent-background = mkLiteral "@red";
          normal-foreground = mkLiteral "@foreground";
          normal-background = mkLiteral "@background";
          active-foreground = mkLiteral "@blue";
          active-background = mkLiteral "@background";
          urgent-foreground = mkLiteral "@red";
          urgent-background = mkLiteral "@background";
          alternate-normal-foreground = mkLiteral "@foreground";
          alternate-normal-background = mkLiteral "@lightbg";
          alternate-active-foreground = mkLiteral "@blue";
          alternate-active-background = mkLiteral "@lightbg";
          alternate-urgent-foreground = mkLiteral "@red";
          alternate-urgent-background = mkLiteral "@lightbg";

          # Text Colors
          base-text = mkRgb "base05";
          selected-normal-text = mkRgb "base01";
          selected-active-text = mkRgb "base00";
          selected-urgent-text = mkRgb "base00";
          normal-text = mkRgb "base05";
          active-text = mkRgb "base0D";
          urgent-text = mkRgb "base08";
          alternate-normal-text = mkRgb "base05";
          alternate-active-text = mkRgb "base0D";
          alternate-urgent-text = mkRgb "base08";
        };

        window.background-color = mkLiteral "@background";

        message.border-color = mkLiteral "@separatorcolor";

        textbox.text-color = mkLiteral "@base-text";

        listview.border-color = mkLiteral "@separatorcolor";

        element-text = {
          background-color = mkLiteral "inherit";
          text-color = mkLiteral "inherit";
        };

        element-icon = {
          background-color = mkLiteral "inherit";
          text-color = mkLiteral "inherit";
        };

        "element normal.normal" = {
          background-color = mkLiteral "@normal-background";
          text-color = mkLiteral "@normal-text";
        };
        "element normal.urgent" = {
          background-color = mkLiteral "@urgent-background";
          text-color = mkLiteral "@urgent-text";
        };
        "element normal.active" = {
          background-color = mkLiteral "@active-background";
          text-color = mkLiteral "@active-text";
        };

        "element selected.normal" = {
          background-color = mkLiteral "@selected-normal-background";
          text-color = mkLiteral "@selected-normal-text";
        };
        "element selected.urgent" = {
          background-color = mkLiteral "@selected-urgent-background";
          text-color = mkLiteral "@selected-urgent-text";
        };
        "element selected.active" = {
          background-color = mkLiteral "@selected-active-background";
          text-color = mkLiteral "@selected-active-text";
        };

        "element alternate.normal" = {
          background-color = mkLiteral "@alternate-normal-background";
          text-color = mkLiteral "@alternate-normal-text";
        };
        "element alternate.urgent" = {
          background-color = mkLiteral "@alternate-urgent-background";
          text-color = mkLiteral "@alternate-urgent-text";
        };
        "element alternate.active" = {
          background-color = mkLiteral "@alternate-active-background";
          text-color = mkLiteral "@alternate-active-text";
        };

        scrollbar.handle-color = mkLiteral "@normal-foreground";
        sidebar.border-color = mkLiteral "@separatorcolor";
        button.text-color = mkLiteral "@normal-text";
        "button selected" = {
          background-color = mkLiteral "@selected-normal-background";
          text-color = mkLiteral "@selected-normal-text";
        };

        inputbar.text-color = mkLiteral "@normal-text";
        case-indicator.text-color = mkLiteral "@normal-text";
        entry.text-color = mkLiteral "@normal-text";
        prompt.text-color = mkLiteral "@normal-text";

        textbox-prompt-colon.text-color = mkLiteral "inherit";
      };
    };
  };
}
