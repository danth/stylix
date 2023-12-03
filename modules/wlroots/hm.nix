{ config, lib, pkgs, ... }:

with config.stylix.colors;

let
  swwwUnits = serviceConfig: {
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
      Service = serviceConfig;
    };
  };

  mpvUnits = file: {
    mpv-wallpaper = {
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
      Unit = {
        Description = "Video wallpaper player";
        Documentation = [ "https://github.com/GhostNaN/mpvpaper#readme" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service.ExecStart = "${pkgs.mpvpaper}/bin/mpvpaper --auto-pause --mpv-options 'no-audio loop' '*' ${file}";
    };
  };

  units = config.stylix.wallpaper.unpack rec {
    image =
      { file, ... }:
      swwwUnits {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.swww}/bin/swww img ${file}";
        Environment = [ "SWWW_TRANSITION=none" ];
      };

    slideshow =
      { files, delay, ... }:
      let slideshow = pkgs.writeShellApplication {
        name = "slideshow";
        runtimeInputs = [ pkgs.swww ];
        text = ''
          images=(${builtins.toString files})
          while true; do
            swww img ''${images[ $RANDOM % ''${#images[@]} ]}
            sleep ${builtins.toString delay}
          done
        '';
      };
      in swwwUnits {
        ExecStart = "${slideshow}/bin/slideshow";
      };

    animation = image;

    video = { file, ... }: mpvUnits file;
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

  config.systemd.user.services =
    lib.mkIf config.stylix.targets.wlroots.enable units;
}
