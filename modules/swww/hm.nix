{ config, lib, pkgs, ... }:

with config.stylix.colors;

let
  swww-init = pkgs.writeShellApplication {
          name = "init";
          runtimeInputs = [ pkgs.swww ]; 
          text = "swww init";
  };
  slideshow-script = pkgs.writeShellApplication {
    name = "slideshow";
    runtimeInputs = [pkgs.swww];
    text = ''
      export SWWW_TRANSITION_FPS=60
      export SWWW_TRANSITION_STEP=2
      # This controls (in seconds) when to switch to the next image
      INTERVAL=${builtins.toString config.stylix.wallpaper.delay}
      imagearray=(${builtins.toString config.stylix.wallpaper.images})
  
      while true; do
      	swww img ''${imagearray[ $RANDOM % ''${#imagearray[@]} ]}
      	sleep $INTERVAL
      done
    '';
  };

in {
  options.stylix.targets.swww.enable =
    config.lib.stylix.mkEnableTarget
    "the desktop background using Swww"
    (with wayland.windowManager; sway.enable 
                              || hyprland.enable);

  config.systemd.user.services.wlroots-wallpaper = {
      Unit = {
        Description = "the stylix wlroots wallpaper systemd service";
        Requires = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
      };
      Service = {
        Type = "forking";
        ExecStart = "${swww-init}/bin/init";        
        ExecStartPost = if (config.lib.stylix.isStatic config.stylix.wallpaper) then "${pkgs.swww}/bin/swww img ${config.stylix.wallpaper.image}"
                        else if (config.lib.stylix.isAnimation config.stylix.wallpaper) then "${pkgs.swww}/bin/swww img ${config.stylix.wallpaper.animation}"
                        else if (config.lib.stylix.isSlideshow config.stylix.wallpaper) then "${slideshow-script}/bin/slideshow" else  "";
        ExecStop = "${pkgs.swww}/bin/swww kill";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
}
