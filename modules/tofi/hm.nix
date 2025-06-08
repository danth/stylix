{ mkTarget, lib, ... }:
mkTarget {
  name = "tofi";
  humanName = "Tofi";

  configElements = [
    (
      { fonts }:
      {
        programs.tofi.settings = {
          font = fonts.monospace.name;
          font-size = toString fonts.sizes.popups;
        };
      }
    )
    (
      { colors, opacity }:
      {
        programs.tofi.settings =
          with colors.withHashtag;
          let
            opacity' = lib.toHexString (builtins.ceil (opacity.popups * 255));
            background = base00 + opacity';
            foreground = base05;
            darkForeground = base04 + opacity';
            selection = base03 + opacity';
          in
          {
            background-color = background;
            outline-color = darkForeground;
            border-color = foreground;
            text-color = foreground;
            prompt-color = base0A;
            prompt-background = background;
            placeholder-color = selection;
            input-background = background;
            default-result-background = background;
            selection-color = selection;
            selection-background = background;
            border-width = lib.mkDefault 4;
            outline-width = lib.mkDefault 2;
          };
      }
    )
  ];
}
