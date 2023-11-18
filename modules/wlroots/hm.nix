{ config, lib, pkgs, ... }:

with config.stylix.colors;

let
  slideshow = pkgs.writeShellApplication {
    name = "slideshow";
    runtimeInputs = [ pkgs.swww ];
    text = ''
      images=(${builtins.toString config.stylix.wallpaper.images})
      while true; do
        swww img ''${images[ $RANDOM % ''${#images[@]} ]}
        sleep ${builtins.toString config.stylix.wallpaper.delay}
      done
    '';
  };

  swww = {
    Unit = {
      Description = "Wallpaper daemon";
      Documentation = [ "https://github.com/Horus645/swww#readme" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Type = "notify";
      NotifyAccess = "all";
      Environment = [ "PATH=${pkgs.swww}/bin" ];
      ExecStart = "${pkgs.swww}/bin/swww init --no-daemon";
      ExecStop = "${pkgs.swww}/bin/swww kill";
    };
  };

  swww-wallpaper = {
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
    Unit = {
      Description = "Apply wallpaper";
      BindsTo = [ "swww.service" ];
      After = [ "swww.service" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service =
      if config.lib.stylix.types.slideshow.check config.stylix.wallpaper
      then {
        ExecStart = "${slideshow}/bin/slideshow";
      }
      else {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart =
          if config.lib.stylix.types.animation.check config.stylix.wallpaper
          then "${pkgs.swww}/bin/swww img ${config.stylix.wallpaper.animation}"
          else "${pkgs.swww}/bin/swww img ${config.stylix.wallpaper.image}";
        Environment = [ "SWWW_TRANSITION=none" ];
      };
  };

  mpv-wallpaper = {
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
    Unit = {
      Description = "Video wallpaper player";
      Documentation = [ "https://github.com/GhostNaN/mpvpaper#readme" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service.ExecStart = "${pkgs.mpvpaper}/bin/mpvpaper --auto-pause --mpv-options 'no-audio loop' '*' ${config.stylix.wallpaper.video}";
  };

in {
  options.stylix.targets.wlroots.enable = lib.mkOption {
    description = lib.mdDoc ''
      Whether to install the wallpaper as a user service.

      This is suitable for compositors based on
      [wlroots](https://gitlab.freedesktop.org/wlroots/wlroots).
    '';
    type = lib.types.bool;
    default = false;
    internal = true;
  };

  config.systemd.user.services = lib.mkIf config.stylix.targets.wlroots.enable {
    swww = lib.mkIf (!config.lib.stylix.types.video.check config.stylix.wallpaper) swww;
    wallpaper =
      if config.lib.stylix.types.video.check config.stylix.wallpaper
      then mpv-wallpaper
      else swww-wallpaper;
  };
}
