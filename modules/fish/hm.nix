{ mkTarget, ... }:
mkTarget {
  name = "fish";
  humanName = "Fish";

  configElements =
    { callElement }:
    {
      programs.fish.interactiveShellInit = callElement ./prompt.nix;
    };
}
