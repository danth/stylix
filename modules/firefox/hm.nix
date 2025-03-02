{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.lib.stylix.templates) firefox-gnome-theme;
  targets = [
    {
      path = "firefox";
      name = "Firefox";
    }
    {
      path = "librewolf";
      name = "LibreWolf";
    }
    {
      path = "floorp";
      name = "Floorp";
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
  mkColor =
    color:
    let
      inherit (config.lib.stylix) colors;
    in
    {
      r = colors."${color}-rgb-r";
      g = colors."${color}-rgb-g";
      b = colors."${color}-rgb-b";
    };
  nur = config.lib.stylix.templates.nur.legacyPackages.${pkgs.system};
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

        colorTheme.enable = config.lib.stylix.mkEnableTarget ''
          [Firefox Color](https://color.firefox.com/) theme
        '' false;

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

          userChrome =
            let
              template = config.lib.stylix.colors {
                template = ./userChrome.mustache;
                extension = "css";
              };
            in
            ''
              @import "${firefox-gnome-theme}/userChrome.css";
              @import "${template}";
            '';

          userContent = ''
            @import "${firefox-gnome-theme}/userContent.css";
          '';
        })
        (lib.mkIf cfg.colorTheme.enable {
          extensions = {
            packages = [ nur.repos.rycee.firefox-addons.firefox-color ];
            settings."FirefoxColor@mozilla.com".settings = {
              firstRunDone = true;
              theme = {
                title = "Stylix ${config.lib.stylix.colors.description}";
                images.additional_backgrounds = [ "./bg-000.svg" ];
                colors = {
                  toolbar = mkColor "base00";
                  toolbar_text = mkColor "base05";
                  frame = mkColor "base01";
                  tab_background_text = mkColor "base05";
                  toolbar_field = mkColor "base02";
                  toolbar_field_text = mkColor "base05";
                  tab_line = mkColor "base0D";
                  popup = mkColor "base00";
                  popup_text = mkColor "base05";
                  button_background_active = mkColor "base04";
                  frame_inactive = mkColor "base00";
                  icons_attention = mkColor "base0D";
                  icons = mkColor "base05";
                  ntp_background = mkColor "base00";
                  ntp_text = mkColor "base05";
                  popup_border = mkColor "base0D";
                  popup_highlight_text = mkColor "base05";
                  popup_highlight = mkColor "base04";
                  sidebar_border = mkColor "base0D";
                  sidebar_highlight_text = mkColor "base05";
                  sidebar_highlight = mkColor "base0D";
                  sidebar_text = mkColor "base05";
                  sidebar = mkColor "base00";
                  tab_background_separator = mkColor "base0D";
                  tab_loading = mkColor "base05";
                  tab_selected = mkColor "base00";
                  tab_text = mkColor "base05";
                  toolbar_bottom_separator = mkColor "base00";
                  toolbar_field_border_focus = mkColor "base0D";
                  toolbar_field_border = mkColor "base00";
                  toolbar_field_focus = mkColor "base00";
                  toolbar_field_highlight_text = mkColor "base00";
                  toolbar_field_highlight = mkColor "base0D";
                  toolbar_field_separator = mkColor "base0D";
                  toolbar_vertical_separator = mkColor "base0D";
                };
              };
            };
          };
        })
      ];
    }) cfg.profileNames
  );
}
