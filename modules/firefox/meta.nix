{ lib, ... }:
{
  maintainers = with lib.maintainers; [
    Flameopathic
    danth
  ];

  name = "Firefox and its derivatives";
}
