{ config, lib, ... }:

with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;
let
  bemenuOpacity = lib.toHexString (
    ((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100
  );
in
{
  options.stylix.targets.bemenu = {
    enable = config.lib.stylix.mkEnableTarget "bemenu" true;

    fontSize = lib.mkOption {
      description = ''
        Font size used for bemenu.
      '';
      type = with lib.types; nullOr int;
      default = sizes.popups;
    }; # optional argument

    alternate = lib.mkOption {
      description = ''
        Whether to use alternating colours.
      '';
      type = lib.types.bool;
      default = false;
    };
  };

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.bemenu.enable)
      {
        programs.bemenu.settings = with config.stylix.targets.bemenu; {
          tb = "${base01}${bemenuOpacity}"; # Title bg
          nb = "${base01}${bemenuOpacity}"; # Normal bg
          fb = "${base01}${bemenuOpacity}"; # Filter bg
          hb = "${base03}${bemenuOpacity}"; # Highlighted bg
          sb = "${base03}${bemenuOpacity}"; # Selected bg
          scb = "${base01}"; # Scrollbar bg

          hf = "${base0A}"; # Highlighted fg
          sf = "${base0B}"; # Selected fg
          tf = "${base05}"; # Title fg
          ff = "${base05}"; # Filter fg
          nf = "${base05}"; # Normal fg
          scf = "${base03}"; # Scrollbar fg

          ab = "${if alternate then base00 else base01}"; # Alternate bg
          af = "${if alternate then base04 else base05}"; # Alternate fg

          # Font name
          fn = "${sansSerif.name} ${
            lib.optionalString (fontSize != null) (builtins.toString fontSize)
          }";
        };
      };
}
