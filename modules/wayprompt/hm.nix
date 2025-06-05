{ mkTarget, lib, ... }:
mkTarget {
  name = "wayprompt";
  humanName = "Wayprompt";

  configElements =
    { colors, opacity }:
    let
      opacity' = lib.toHexString (builtins.ceil (opacity.popups * 255));
    in
    {
      programs.wayprompt.settings.colours = with colors; {
        background = "${base00}${opacity'}";
        border = base0D;
        text = base05;
        error-text = base08;

        pin-background = base01;
        pin-border = base05;
        pin-square = base05;

        ok-button = green;
        ok-button-border = green;
        ok-button-text = base00;

        not-ok-button = yellow;
        not-ok-button-border = yellow;
        not-ok-button-text = base00;

        cancel-button = red;
        cancel-button-border = red;
        cancel-button-text = base00;
      };
    };
}
