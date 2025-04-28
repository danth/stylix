{
  config,
  lib,
  options,
  ...
}:
let
  template = import ./template.nix {
    inherit (config.lib.stylix) colors;
    inherit (config.stylix) fonts;
  };
in
{
  imports = lib.singleton (
    lib.mkRemovedOptionModule [ "stylix" "targets" "vesktop" "extraCss" ]
      "CSS can be added to by declaring 'programs.vesktop.vencord.themes.stylix = lib.mkAfter \"YOUR EXTRA CSS\";"
  );
  options.stylix.targets = {
    vesktop.enable = config.lib.stylix.mkEnableTarget "Vesktop" true;
    vencord = {
      enable = config.lib.stylix.mkEnableTarget "Vencord" true;
      extraCss = lib.mkOption {
        description = "Extra CSS to added to Vencord's theme";
        type = lib.types.lines;
        default = "";
      };
    };
    nixcord = {
      enable = config.lib.stylix.mkEnableTarget "Nixcord" true;
      extraCss = lib.mkOption {
        description = "Extra CSS to added to Nixcord's theme";
        type = lib.types.lines;
        default = "";
      };
    };
  };

  config = lib.mkIf config.stylix.enable (
    lib.mkMerge [
      (lib.mkIf config.stylix.targets.vencord.enable {
        xdg.configFile."Vencord/themes/stylix.theme.css".text =
          template + config.stylix.targets.vencord.extraCss;
      })

      (lib.mkIf config.stylix.targets.vesktop.enable {
        programs.vesktop.vencord = {
          themes.stylix = template;
          settings.enabledThemes = [ "stylix.css" ];
        };
      })

      (lib.mkIf config.stylix.targets.nixcord.enable (
        lib.optionalAttrs (builtins.hasAttr "nixcord" options.programs) {
          xdg.configFile =
            let
              inherit (config.programs) nixcord;
            in
            lib.mkMerge [
              (lib.mkIf nixcord.discord.enable {
                "Vencord/themes/stylix.theme.css".text =
                  template + config.stylix.targets.nixcord.extraCss;
              })

              (lib.mkIf nixcord.vesktop.enable {
                "vesktop/themes/stylix.theme.css".text =
                  template + config.stylix.targets.nixcord.extraCss;
              })
            ];

          programs.nixcord.config.enabledThemes = [ "stylix.theme.css" ];
        }
      ))
    ]
  );
}
