{
  lib,
  pkgs,
  config,
  ...
}:
let
  pkg = pkgs.wayprompt;
  waypromptCmd = "'${lib.getExe' pkg "wayprompt"}' --title \"Stylix test\" --description \"Lorem ipsum dolor sit amet, consectetur adipiscing elit.\" --prompt \"Enter your password\" --button-ok \"OK\" --button-not-ok \"Not OK\" --button-cancel \"Cancel\" --get-pin";
in
{
  programs.sway.enable = true;

  services.greetd = {
    enable = true;
    settings.default_session = {
      command = lib.escapeShellArg (lib.getExe config.programs.sway.package);
      user = "guest";
    };
  };

  home-manager.sharedModules = lib.singleton {
    programs.wayprompt = {
      enable = true;
      package = pkg;
    };

    wayland.windowManager.sway = {
      enable = true;
      config.startup = [
        { command = waypromptCmd; }
      ];
    };
  };
}
