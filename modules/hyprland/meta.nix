{ lib, ... }:
{
  name = "Hyprland";
  homepages = "https://github.com/hyprwm/Hyprland";
  maintainers = with lib.maintainers; [
    naho
    skoove
  ];
}
