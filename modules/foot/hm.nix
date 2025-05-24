{ mkTarget, ... }:
mkTarget {
  name = "foot";
  humanName = "Foot";

  configElements = [
    (
      { fonts }:
      {
        programs.foot.settings.main = {
          font = "${fonts.monospace.name}:size=${toString fonts.sizes.terminal}";
          dpi-aware = "no";
        };
      }
    )
    (
      { opacity }:
      {
        programs.foot.settings.colors.alpha = opacity.terminal;
      }
    )
    (
      { colors, inputs }:
      {
        programs.foot.settings.main.include = toString (colors {
          templateRepo = inputs.tinted-foot;
        });
      }
    )
  ];
}
