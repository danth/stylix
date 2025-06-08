{ mkTarget, ... }:
mkTarget {
  name = "console";
  humanName = "the Linux kernel console";

  configElements =
    { colors }:
    {
      console.colors = with colors; [
        base00-hex
        red
        green
        yellow
        blue
        magenta
        cyan
        base05-hex
        base03-hex
        red
        green
        yellow
        blue
        magenta
        cyan
        base07-hex
      ];
    };
}
