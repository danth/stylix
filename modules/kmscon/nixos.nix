{ mkTarget, ... }:
mkTarget {
  name = "kmscon";
  humanName = "the kmscon virtual console";

  configElements = [
    (
      { fonts }:
      {
        services.kmscon = {
          fonts = [ fonts.monospace ];
          extraConfig = "font-size=${toString fonts.sizes.terminal}";
        };
      }
    )
    (
      { colors }:
      {
        services.kmscon.extraConfig =
          let
            formatBase =
              name:
              let
                getComponent = comp: colors."${name}-rgb-${comp}";
              in
              "${getComponent "r"},${getComponent "g"},${getComponent "b"}";
          in
          ''
            palette=custom

            palette-black=${formatBase "base00"}
            palette-red=${formatBase "base08"}
            palette-green=${formatBase "base0B"}
            palette-yellow=${formatBase "base0A"}
            palette-blue=${formatBase "base0D"}
            palette-magenta=${formatBase "base0E"}
            palette-cyan=${formatBase "base0C"}
            palette-light-grey=${formatBase "base05"}
            palette-dark-grey=${formatBase "base03"}
            palette-light-red=${formatBase "base08"}
            palette-light-green=${formatBase "base0B"}
            palette-light-yellow=${formatBase "base0A"}
            palette-light-blue=${formatBase "base0D"}
            palette-light-magenta=${formatBase "base0E"}
            palette-light-cyan=${formatBase "base0C"}
            palette-white=${formatBase "base07"}

            palette-background=${formatBase "base00"}
            palette-foreground=${formatBase "base05"}
          '';
      }
    )
  ];
}
