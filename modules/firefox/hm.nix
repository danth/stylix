{
  lib,
  mkTarget,
  ...
}:
{
  imports =
    map
      (
        { name, humanName }:
        lib.modules.importApply ./each-config.nix {
          inherit
            mkTarget
            name
            humanName
            ;
        }
      )
      [
        {
          name = "firefox";
          humanName = "Firefox";
        }
        {
          name = "librewolf";
          humanName = "LibreWolf";
        }
        {
          name = "floorp";
          humanName = "Floorp";
        }
      ];
}
