{
  config,
  lib,
  pkgs,
  options,
  ...
}:
let
  template =
    let
      inherit (config.lib.stylix) colors;
      inherit (config.stylix) fonts;
    in
    import ./template.nix { inherit colors fonts; };
in
{
  options.stylix.targets =
    let
      inherit (config.lib.stylix) mkEnableTarget;
    in
    {
      vencord.enable = mkEnableTarget "Vencord" true;
      vesktop.enable = mkEnableTarget "Vesktop" true;
      nixcord.enable = mkEnableTarget "Nixcord" true;
    };

  config = lib.mkIf config.stylix.enable (
    lib.mkMerge [
      (lib.mkIf config.stylix.targets.vencord.enable {
        xdg.configFile."Vencord/themes/stylix.theme.css".text = template;
      })

      (lib.mkIf config.stylix.targets.vesktop.enable (
        lib.mkMerge [
          (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
            xdg.configFile."vesktop/themes/stylix.theme.css".text = template;
          })

          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
            home.file."Library/Application Support/vesktop/themes/stylix.theme.css".text =
              template;
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
                "Vencord/themes/stylix.theme.css".text = template;
              })

              (lib.mkIf nixcord.vesktop.enable {
                "vesktop/themes/stylix.theme.css".text = template;
              })
            ];

          programs.nixcord.config.enabledThemes = [ "stylix.theme.css" ];
        }
      ))
    ]
  );
}
