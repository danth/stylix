{pkgs, config, lib, ... }:

with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;
{
  options.stylix.targets.bemenu = {
    enable = config.lib.stylix.mkEnableTarget "bemenu" true;
    fontSize = lib.mkOption {
      type = with lib.types; nullOr int;
      default = sizes.popups;
    }; # optional argument
    alternate = lib.mkOption { type = lib.types.bool; default = false; };
  };

  config = lib.mkIf config.stylix.targets.bemenu.enable {
    home.sessionVariables.BEMENU_OPTS = with config.stylix.targets.bemenu; builtins.concatStringsSep " " [
      # Inspired from https://git.sr.ht/~h4n1/base16-bemenu_opts
      "--tb '${base01}'"
      "--nb '${base01}'"
      "--fb '${base01}'"
      "--hb '${base03}'"
      "--sb '${base03}'"
      "--hf '${base0A}'"
      "--sf '${base0B}'"
      "--tf '${base05}'"
      "--ff '${base05}'"
      "--nf '${base05}'"
      "--scb '${base01}'"
      "--scf '${base03}'"
      # Alternating colours, currently set to match primary. Adding a module option to enable or disable alternating in Stylix could be useful
      "--ab '${if alternate then base00 else base01}'"
      "--af '${if alternate then base04 else base05}'"

      "--fn '${sansSerif.name} ${lib.optionalString (fontSize != null) (builtins.toString fontSize)}'" 
    ];
  };
}
