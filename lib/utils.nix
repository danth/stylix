{ config, lib, pkgs, ... }:

{
  config.lib.stylix = {
    # get the opacity values as strings in hexadecimal, integer and floating point values for the four opacity options
    desktopOpacity-hex = lib.toHexString ((((builtins.ceil (config.stylix.opacity.desktop * 100)) * 255) / 100));
    desktopOpacity-int = builtins.toString (builtins.ceil (config.stylix.opacity.desktop * 100));
    desktopOpacity-float = builtins.toString config.stylix.opacity.desktop;


    applicationsOpacity-hex = lib.toHexString ((((builtins.ceil (config.stylix.opacity.applications * 100)) * 255) / 100));
    applicationsOpacity-int = builtins.toString (builtins.ceil (config.stylix.opacity.applications * 100));
    applicationsOpacity-float = builtins.toString config.stylix.opacity.applications;


    terminalOpacity-hex = lib.toHexString ((((builtins.ceil (config.stylix.opacity.terminal * 100)) * 255) / 100));
    terminalOpacity-int = builtins.toString (builtins.ceil (config.stylix.opacity.terminal * 100));
    terminalOpacity-float = builtins.toString config.stylix.opacity.terminal;


    popupsOpacity-hex = lib.toHexString ((((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100));
    popupsOpacity-int = builtins.toString (builtins.ceil (config.stylix.opacity.popups * 100));
    popupsOpacity-float = builtins.toString config.stylix.opacity.popups;

    backgroundPolarity =
      let
        red = lib.toInt config.stylix.colors.base00-rgb-r;
        green = lib.toInt config.stylix.colors.base00-rgb-g;
        blue = lib.toInt config.stylix.colors.base00-rgb-b;
      in
      if (red + green + blue >= 150) then "light" else "dark";

    # Generate a PNG image containing a named color
    waylandSlideshowScript = pkgs.writeScript "script.sh" ''
      export SWWW_TRANSITION_FPS=60
      export SWWW_TRANSITION_STEP=2

      # This controls (in seconds) when to switch to the next image
      INTERVAL=${builtins.toString config.stylix.wallpaper.delay}
      imagearray=(${builtins.toString config.stylix.wallpaper.images})
  
      while true; do
      	${pkgs.swww}/bin/swww img ''${imagearray[ $RANDOM % ''${#imagearray[@]} ]}
      	sleep $INTERVAL
      done
    '';

    solid = color: pkgs.runCommand "${color}-pixel.png" { } "${pkgs.imagemagick}/bin/convert xc:#${color} png32:$out";
    pixel = color:
      pkgs.runCommand "${color}-pixel.png"
        {
          color = config.stylix.colors.withHashtag.${color};
        } "${pkgs.imagemagick}/bin/convert xc:$color png32:$out";
  };
}
