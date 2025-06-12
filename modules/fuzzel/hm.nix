{ mkTarget, lib, ... }:
mkTarget {
  name = "fuzzel";
  humanName = "Fuzzel";

  configElements = [
    (
      { fonts }:
      {
        programs.fuzzel.settings.main.font =
          "${fonts.sansSerif.name}:size=${toString fonts.sizes.popups}";
      }
    )
    (
      { colors, opacity }:
      let
        opacity' = lib.toHexString (builtins.ceil (opacity.popups * 255));
      in
      {
        programs.fuzzel.settings.colors = with colors; {
          background = "${base00-hex}${opacity'}";
          text = "${base05-hex}ff";
          placeholder = "${base03-hex}ff";
          prompt = "${base05-hex}ff";
          input = "${base05-hex}ff";
          match = "${base0A-hex}ff";
          selection = "${base03-hex}ff";
          selection-text = "${base05-hex}ff";
          selection-match = "${base0A-hex}ff";
          counter = "${base06-hex}ff";
          border = "${base0D-hex}ff";
        };
      }
    )
    (
      { polarity, iconTheme }:
      {
        programs.fuzzel.settings.main."icon-theme" =
          if (polarity == "dark") then iconTheme.dark else iconTheme.light;
      }
    )
  ];
}
