{ mkTarget, ... }:
mkTarget {
  name = "fish";
  humanName = "Fish";

  configElements =
    { colors, inputs }:
    {
      programs.fish.promptInit = import ./prompt.nix { inherit colors inputs; };
    };
}
