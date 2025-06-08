# manually applied args
{
  mkTarget,
  name,
  humanName,
}:
# module args
{
  lib,
  pkgs,
  config,
  ...
}:
mkTarget {
  inherit name humanName;

  extraOptions = {
    profileNames = lib.mkOption {
      description = "The ${humanName} profile names to apply styling on.";
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };

    colorTheme.enable = lib.mkEnableOption "[Firefox Color](https://color.firefox.com/) on ${humanName}";

    firefoxGnomeTheme.enable = lib.mkEnableOption "[Firefox GNOME theme](https://github.com/rafaelmardojai/firefox-gnome-theme) on ${humanName}";
  };

  configElements = [
    (
      { cfg }:
      {
        warnings =
          lib.optional (config.programs.${name}.enable && cfg.profileNames == [ ])
            ''stylix: ${name}: `config.stylix.targets.${name}.profileNames` is not set. Declare profile names with 'config.stylix.targets.${name}.profileNames = [ "<PROFILE_NAME>" ];'.'';
      }
    )
    (
      { cfg, fonts }:
      {
        programs.${name}.profiles = lib.genAttrs cfg.profileNames (_: {
          settings = {
            "font.name.monospace.x-western" = fonts.monospace.name;
            "font.name.sans-serif.x-western" = fonts.sansSerif.name;
            "font.name.serif.x-western" = fonts.serif.name;
          };
        });
      }
    )
    (
      {
        cfg,
        colors,
        inputs,
      }:
      {
        programs.${name}.profiles = lib.mkIf cfg.firefoxGnomeTheme.enable (
          lib.genAttrs cfg.profileNames (_: {
            settings = {
              "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
              "svg.context-properties.content.enabled" = true;
            };

            userChrome =
              let
                template = colors {
                  template = ./userChrome.css.mustache;
                  extension = ".css";
                };
              in
              ''
                @import "${inputs.firefox-gnome-theme}/userChrome.css";
                @import "${template}";
              '';

            userContent = ''
              @import "${inputs.firefox-gnome-theme}/userContent.css";
            '';
          })
        );
      }
    )
    (
      {
        cfg,
        colors,
        inputs,
      }:
      let
        mkColor = color: {
          r = colors."${color}-rgb-r";
          g = colors."${color}-rgb-g";
          b = colors."${color}-rgb-b";
        };
        inherit (pkgs.stdenv.hostPlatform) system;
        inherit (inputs.nur.legacyPackages.${system}.repos.rycee)
          firefox-addons
          ;
      in
      {
        programs.${name}.profiles = lib.mkIf cfg.colorTheme.enable (
          lib.genAttrs cfg.profileNames (_: {
            extensions = {
              packages = [ firefox-addons.firefox-color ];
              settings."FirefoxColor@mozilla.com".settings = {
                firstRunDone = true;
                theme = {
                  title = "Stylix ${colors.description}";
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
        );
      }
    )
  ];
}
