{ mkTarget, ... }:
mkTarget {
  name = "hyprpaper";
  humanName = "Hyprpaper";

  configElements =
    { image }:
    {
      services.hyprpaper.settings = {
        preload = [ "${image}" ];
        wallpaper = [ ",${image}" ];
      };
    };
}
