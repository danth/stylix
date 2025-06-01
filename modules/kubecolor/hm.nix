{ mkTarget, ... }:
mkTarget {
  name = "kubecolor";
  humanName = "kubecolor";

  configElements = [
    (
      { polarity }:
      {
        programs.kubecolor.settings.preset =
          if polarity == "either" then "" else polarity;
      }
    )
    (
      { colors }:
      {
        programs.kubecolor.settings.theme = with colors.withHashtag; {
          base = {
            info = "fg=${base05-hex}";
            primary = "fg=${base0E-hex}";
            secondary = "fg=${base0D-hex}";
            success = "fg=${base0B-hex}:bold";
            warning = "fg=${base0A-hex}:bold";
            danger = "fg=${base08-hex}:bold";
            muted = "fg=${base04-hex}";
            key = "fg=${base07-hex}:bold";
          };
          default = "fg=${base05-hex}";
          data = {
            key = "fg=${base07-hex}:bold";
            string = "fg=${base05-hex}";
            true = "fg=${base0B-hex}:bold";
            false = "fg=${base08-hex}:bold";
            number = "fg=${base0E-hex}";
            null = "fg=${base04-hex}";
            quantity = "fg=${base0E-hex}";
            duration = "fg=${base09-hex}";
            durationfresh = "fg=${base0B-hex}";
            ratio = {
              zero = "fg=${base04-hex}";
              equal = "fg=${base0B-hex}";
              unequal = "fg=${base0A-hex}";
            };
          };
          status = {
            success = "fg=${base0B-hex}:bold";
            warning = "fg=${base0A-hex}:bold";
            error = "fg=${base08-hex}:bold";
          };
          table = {
            header = "fg=${base05-hex}:bold";
            columns = "fg=${base05-hex}";
          };
          stderr = {
            default = "fg=${base05-hex}";
            error = "fg=${base08-hex}:bold";
          };
          describe = {
            key = "fg=${base07-hex}:bold";
          };
          apply = {
            created = "fg=${base0B-hex}";
            configured = "fg=${base0A-hex}";
            unchanged = "fg=${base05-hex}";
            dryrun = "fg=${base0D-hex}";
            fallback = "fg=${base05-hex}";
          };
          explain = {
            key = "fg=${base07-hex}:bold";
            required = "fg=${base00-hex}:bold";
          };
          options = {
            flag = "fg=${base07-hex}:bold";
          };
          version = {
            key = "fg=${base07-hex}:bold";
          };
        };
      }
    )
  ];
}
