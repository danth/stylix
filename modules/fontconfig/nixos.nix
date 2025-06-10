{ lib, mkTarget, ... }:
{
  imports = [
    (lib.modules.importApply ./fontconfig.nix { inherit mkTarget; })
  ];
}
