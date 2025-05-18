{ lib, ... }:
{
  maintainers = with lib.maintainers; [
    Flameopathic
    danth
  ];

  name = "VSCode";
}
