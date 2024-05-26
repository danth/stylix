{ pkgs, config, lib, ... }:

with config.lib.stylix.colors.withHashtag;
let
  inherit (config.stylix) fonts;
  inherit (fonts) sizes;
  sansSerif = builtins.head fonts.sansSerif;
  bemenuOpacity = lib.toHexString ((((builtins.ceil (config.stylix.opacity.popups * 100)) * 255) / 100));
in {
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

  config = lib.mkIf config.stylix.targets.bemenu.enable {
    home.sessionVariables.BEMENU_OPTS = with config.stylix.targets.bemenu; builtins.concatStringsSep " " [
      # Inspired from https://git.sr.ht/~h4n1/base16-bemenu_opts
      "--tb '${base01}${bemenuOpacity}'"
      "--nb '${base01}${bemenuOpacity}'"
      "--fb '${base01}${bemenuOpacity}'"
      "--hb '${base03}${bemenuOpacity}'"
      "--sb '${base03}${bemenuOpacity}'"
      "--hf '${base0A}'"
      "--sf '${base0B}'"
      "--tf '${base05}'"
      "--ff '${base05}'"
      "--nf '${base05}'"
      "--scb '${base01}'"
      "--scf '${base03}'"
      "--ab '${if alternate then base00 else base01}'"
      "--af '${if alternate then base04 else base05}'"
      "--fn '${sansSerif.name} ${lib.optionalString (fontSize != null) (builtins.toString fontSize)}'"
    ];
  };
}
