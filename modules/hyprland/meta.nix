{ lib, ... }:
{
  maintainers = with lib.maintainers; [
    naho
    skoove
  ];

  name = "Hyprland";
}
