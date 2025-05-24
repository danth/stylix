{ mkTarget, ... }:
mkTarget {
  name = "fish";
  humanName = "Fish";

  configElements =
    { colors, inputs }:
    {
      programs.fish.interactiveShellInit = import ./prompt.nix {
        inherit colors inputs;
      };
    };
}
