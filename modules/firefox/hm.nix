{ config, lib, ... }:

let
  targets = [
    {
      path = "firefox";
      name = "Firefox";
    }
    {
      path = "librewolf";
      name = "LibreWolf";
    }
  ];
  eachConfig = mkCfg: targets: lib.mkMerge (map mkCfg targets);
  eachTarget =
    mkCfg:
    lib.mkIf config.stylix.enable (
      eachConfig (
        target:
        let
          cfg = config.stylix.targets.${target.path};
          programCfg = config.programs.${target.path};
        in
        lib.mkIf cfg.enable (mkCfg {
          inherit target cfg programCfg;
        })
      ) targets
    );
in
{
  options.stylix.targets = lib.listToAttrs (
    map (
      target:
      lib.nameValuePair target.path {
        enable = config.lib.stylix.mkEnableTarget target.name true;

        profileNames = lib.mkOption {
          description = "The ${target.name} profile names to apply styling on.";
          type = lib.types.listOf lib.types.str;
          default = [ ];
        };

        firefoxGnomeTheme.enable = config.lib.stylix.mkEnableTarget ''
          [Firefox GNOME
          theme](https://github.com/rafaelmardojai/firefox-gnome-theme)
        '' false;
      }
    ) targets
  );

  # This and the below assignment aren't merged because of
  # https://discourse.nixos.org/t/infinite-recursion-in-module-with-mkmerge/10989
  config.programs = eachTarget (
    { target, cfg, ... }:
    eachConfig (profileName: {
      ${target.path}.profiles.${profileName} = lib.mkMerge [
        {
          settings = {
            "font.name.monospace.x-western" = config.stylix.fonts.monospace.name;
            "font.name.sans-serif.x-western" = config.stylix.fonts.sansSerif.name;
            "font.name.serif.x-western" = config.stylix.fonts.serif.name;
          };
        }
        (lib.mkIf cfg.firefoxGnomeTheme.enable {
          settings = {
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            "svg.context-properties.content.enabled" = true;
          };

          userChrome = builtins.readFile (
            config.lib.stylix.colors {
              template = ./userChrome.mustache;
              extension = "css";
            }
          );

          userContent = ''
            @import "firefox-gnome-theme/userContent.css";
          '';
        })
      ];
    }) cfg.profileNames
  );

  config.home.file = eachTarget (
    { cfg, programCfg, ... }:
    lib.mkIf cfg.firefoxGnomeTheme.enable (
      eachConfig (profileName: {
        "${programCfg.configPath}/${profileName}/chrome/firefox-gnome-theme".source =
          config.lib.stylix.templates.firefox-gnome-theme;
      }) cfg.profileNames
    )
  );
}
