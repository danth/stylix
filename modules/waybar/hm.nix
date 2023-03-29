{ config, lib, ... }:
with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;

{
  options.stylix.targets.waybar.enable = config.lib.stylix.mkEnableTarget "Waybar" true;

  config = lib.mkIf config.stylix.targets.waybar.enable {
    programs.waybar.style = ''
      * {
          border: none;
          border-radius: 0;
          font-family: ${sansSerif.name};
          font-size: ${builtins.toString sizes.desktop};
      }
      window#waybar {
          background: ${base00};
          color: ${base05};
      }
      #workspaces button {
          padding: 0 5px;
          background-color: ${base01};
          color: ${base04};
      }
      #workspaces button.focused, #workspaces button.active {
          background: ${base02};
      }
      #workspaces button.urgent {
          background-color: ${base08};
      }
      #wireplumber, #pulseaudio, #sndio {
          background-color: ${base09};
          color: ${base04};
          padding: 0 5px;
      }
      #wireplumber.muted, #pulseaudio.muted, #sndio.muted {
          background-color: ${base0C};
      }
      #upower, #battery {
          background-color: ${base0D};
          color: ${base04};
          padding: 0 5px;
      }
      #upower.charging, #battery.Charging {
          background-color: ${base0E};
      }
      #network {
          background-color: ${base0B};
          color: ${base04};
          padding: 0 5px;
      }
      #network.disconnected {
          background-color: ${base0C};
      }
      #user {
          background-color: ${base0F};
          color: ${base04};
          padding: 0 5px;
      }
      #clock {
          background-color: ${base03};
          color: ${base04};
          padding: 0 5px;
      }
    '';
  };
}
