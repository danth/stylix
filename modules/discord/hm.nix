{
  config,
  lib,
  pkgs,
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
  options.stylix.targets =
    lib.mapAttrs
      (_: prettyName: {
        enable = config.lib.stylix.mkEnableTarget prettyName true;
        extraCss = lib.mkOption {
          description = "Extra CSS to added to ${prettyName}'s theme";
          type = lib.types.lines;
          default = "";
        };
      })
      {
        vencord = "Vencord";
        vesktop = "Vesktop";
        nixcord = "Nixcord";
      };

  config = lib.mkIf config.stylix.enable (
    lib.mkMerge [
      (lib.mkIf config.stylix.targets.vencord.enable {
        xdg.configFile."Vencord/themes/stylix.theme.css".text =
          template + config.stylix.targets.vencord.extraCss;
      })

      (lib.mkIf config.stylix.targets.vesktop.enable (
        lib.mkMerge [
          (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
            xdg.configFile."vesktop/themes/stylix.theme.css".text =
              template + config.stylix.targets.vesktop.extraCss;
          })

          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
            home.file."Library/Application Support/vesktop/themes/stylix.theme.css".text =
              template + config.stylix.targets.vesktop.extraCss;
          })
        ]
      ))

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
