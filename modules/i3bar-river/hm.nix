{
  mkTarget,
  lib,
  ...
}:
mkTarget {
  name = "i3bar-river";
  humanName = "i3bar-river";

  configElements = [
    (
      { fonts }:
      {
        programs.i3bar-river.settings = {
          font = "${fonts.monospace.name} ${toString fonts.sizes.desktop}";
        };
      }
    )
    (
      { colors, opacity }:
      let
        opacityHex = lib.toHexString (builtins.ceil (opacity.desktop * 255));
        c = colors.withHashtag;
      in
      {
        programs.i3bar-river.settings =
          lib.pipe
            {
              background = c.base00;
              color = c.base05;
              separator = c.base01;
              tag_fg = c.base05;
              tag_bg = c.base00;
              tag_focused_fg = c.base00;
              tag_focused_bg = c.base05;
              tag_urgent_fg = c.base00;
              tag_urgent_bg = c.base0A;
              tag_inactive_fg = c.base05;
              tag_inactive_bg = c.base01;
            }
            [
              (lib.mapAttrs (
                n: v: if n == "background" || lib.hasSuffix "_bg" n then v + opacityHex else v
              ))
            ];
      }
    )
  ];
}
