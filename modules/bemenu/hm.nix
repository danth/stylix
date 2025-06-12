{
  mkTarget,
  lib,
  config,
  ...
}:
mkTarget {
  name = "bemenu";
  humanName = "bemenu";

  extraOptions = {
    fontSize = lib.mkOption {
      description = ''
        Font size used for bemenu.
      '';
      type = with lib.types; nullOr int;
      default = config.stylix.fonts.sizes.popups;
    }; # optional argument

    alternate = lib.mkOption {
      description = ''
        Whether to use alternating colours.
      '';
      type = lib.types.bool;
      default = false;
    };
  };

  configElements = [
    (
      { cfg, fonts }:
      {
        programs.bemenu.settings = {
          # Font name
          fn = "${fonts.sansSerif.name} ${
            lib.optionalString (cfg.fontSize != null) (builtins.toString cfg.fontSize)
          }";
        };
      }
    )
    (
      {
        colors,
        opacity,
        cfg,
      }:
      {
        programs.bemenu.settings =
          with colors.withHashtag;
          let
            bemenuOpacity = lib.toHexString (builtins.ceil (opacity.popups * 255));
          in
          {
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

            ab = "${if cfg.alternate then base00 else base01}"; # Alternate bg
            af = "${if cfg.alternate then base04 else base05}"; # Alternate fg
          };
      }
    )
  ];
}
