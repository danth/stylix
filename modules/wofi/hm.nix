{ mkTarget, ... }:
mkTarget {
  name = "wofi";
  humanName = "wofi";

  configElements = [
    (
      { fonts }:
      {
        programs.wofi.style = ''
          window {
            font-family: "${fonts.monospace.name}";
            font-size: ${toString fonts.sizes.popups}pt;
          }
        '';
      }
    )
    (
      { colors }:
      {
        programs.wofi.style = with colors.withHashtag; ''
          window {
            background-color: ${base00};
            color: ${base05};
          }

          #entry:nth-child(odd) {
            background-color: ${base00};
          }

          #entry:nth-child(even) {
            background-color: ${base01};
          }

          #entry:selected {
            background-color: ${base02};
          }

          #input {
            background-color: ${base01};
            color: ${base04};
            border-color: ${base02};
          }

          #input:focus {
            border-color: ${base0A};
          }
        '';
      }
    )
  ];
}
