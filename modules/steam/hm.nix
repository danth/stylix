{ config, lib, pkgs, ... }:

let
  cfg = config.stylix.targets.steam;
in {
  options.stylix.targets.steam = {
    enable = config.lib.stylix.mkEnableTarget "Steam" true;
    adwaitaForSteam.enable = config.lib.stylix.mkEnableTarget "Adwaita for Steam" false;
  };

  config = lib.mkIf (config.stylix.enable && cfg.enable) (lib.mkMerge [
    {
      # Generic configuration
    }

    (lib.mkIf cfg.adwaitaForSteam.enable {
      home.packages = with pkgs; [adwsteamgtk];

      home.activation.adwaitaForSteam = let
        shellScript = pkgs.writeShellScript "adwaitaForSteam" ''
          rm -f "$HOME/.cache/AdwSteamInstaller/extracted/custom/custom.css"
          ${lib.getExe pkgs.adwsteamgtk}  -i
        '';
      in
        config.lib.dag.entryAfter ["writeBoundary" "dconfSettings"] ''
          run --quiet ${shellScript}
        '';

      # Use custom.css
      dconf.settings."io/github/Foldex/AdwSteamGtk".prefs-install-custom-css = true;

      xdg.configFile."AdwSteamGtk/custom.css".source = config.lib.stylix.colors {
        template = ./custom.mustache;
        extension = "css";
      };

    })
  ]);
}
