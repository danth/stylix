{ mkTarget, ... }:
mkTarget {
  name = "tmux";
  humanName = "Tmux";

  configElements =
    { colors, inputs }:
    {
      programs.tmux.extraConfig = ''
        source-file ${
          colors {
            templateRepo = inputs.tinted-tmux;
            target = "base16";
          }
        }
      '';
    };
}
