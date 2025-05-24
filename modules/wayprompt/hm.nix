{ mkTarget, lib, ... }:
mkTarget {
  name = "wayprompt";
  humanName = "Wayprompt";

  configElements = [
    (
      { colors, opacity }:
      let
        opacity' = lib.toHexString (builtins.ceil (opacity.popups * 255));
      in
      {
        programs.wayprompt.settings.colours = with colors; {
          background = "${base00-hex}${opacity'}";
          border = base0D-hex;
          text = base05-hex;
          error-text = base08-hex;

          pin-background = base01-hex;
          pin-border = base05-hex;
          pin-square = base05-hex;

          ok-button = green;
          ok-button-border = green;
          ok-button-text = base00-hex;

          not-ok-button = yellow;
          not-ok-button-border = yellow;
          not-ok-button-text = base00-hex;

          cancel-button = red;
          cancel-button-border = red;
          cancel-button-text = base00-hex;
        };
      }
    )
  ];
}
