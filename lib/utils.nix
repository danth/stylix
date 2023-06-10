{config, lib, pkgs, ...}:

{
  # get the opacity values as strings in hexadecimal, integer and floating point values for the four opacity options
  config.lib.stylix.desktopOpacity-hex = lib.toHexString ((((builtins.ceil (config.stylix.opacity.desktop * 100)) * 255) / 100));
  config.lib.stylix.desktopOpacity-int = builtins.toString (builtins.ceil (config.stylix.opacity.desktop * 100));
  config.lib.stylix.desktopOpacity-float = builtins.toString config.stylix.opacity.desktop;
  
  
  config.lib.stylix.applicationsOpacity-hex = lib.toHexString ((((builtins.ceil (config.stylix.opacity.applications * 100)) * 255) / 100));
  config.lib.stylix.applicationsOpacity-int = builtins.toString (builtins.ceil (config.stylix.opacity.applications * 100));
  config.lib.stylix.applicationsOpacity-float = builtins.toString config.stylix.opacity.applications;
  
  
  config.lib.stylix.terminalOpacity-hex = lib.toHexString ((((builtins.ceil (config.stylix.opacity.terminal * 100)) * 255) / 100));
  config.lib.stylix.terminalOpacity-int = builtins.toString (builtins.ceil (config.stylix.opacity.terminal * 100));
  config.lib.stylix.terminalOpacity-float = builtins.toString config.stylix.opacity.terminal;
  
  
  config.lib.stylix.popupsOpacity-hex = lib.toHexString ((((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100));
  config.lib.stylix.popupsOpacity-int = builtins.toString (builtins.ceil (config.stylix.opacity.popups * 100));
  config.lib.stylix.popupsOpacity-float = builtins.toString config.stylix.opacity.popups;

  # determine polarity for things like web browsers and gnome
  config.lib.stylix.backgroundPolarity = let
    red = lib.toInt config.lib.stylix.colors.base00-rgb-r;
    green = lib.toInt config.lib.stylix.colors.base00-rgb-g;
    blue = lib.toInt config.lib.stylix.colors.base00-rgb-b;
  in if (red + green + blue >= 150) then "light" else "dark";

  # Generate a PNG image containing a named color
  config.lib.stylix.solid = color: pkgs.runCommand "${color}-pixel.png" {} "${pkgs.imagemagick}/bin/convert xc:#${color} png32:$out";
    config.lib.stylix.pixel = color:
        pkgs.runCommand "${color}-pixel.png" {
                  color = config.lib.stylix.colors.withHashtag.${color};
        } "${pkgs.imagemagick}/bin/convert xc:$color png32:$out";
}
