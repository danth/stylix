{ mkTarget, ... }:
mkTarget {
  name = "fish";
  humanName = "Fish";

  configElements =
    { callElement }:
    {
      programs.fish.promptInit = callElement ./prompt.nix;
    };
}
