{ lib, ... }:
{
  name = "VSCode";
  homepage = "https://code.visualstudio.com/";
  maintainers = with lib.maintainers; [
    Flameopathic
    danth
  ];
}
