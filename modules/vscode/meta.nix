{ lib, ... }:
{
  name = "VSCode";
  homepages = "https://code.visualstudio.com/";
  maintainers = with lib.maintainers; [
    Flameopathic
    danth
  ];
}
