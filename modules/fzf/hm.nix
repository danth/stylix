{ pkgs, config, lib, ... }:
let
  mkFzfKeyValue = lib.generators.mkKeyValueDefault { } ":";

  colorConfig = with config.lib.stylix.colors.withHashtag;
    lib.concatStringsSep "," (lib.mapAttrsToList mkFzfKeyValue {
      "bg" = base00;
      "bg+" = base01;
      "fg" = base04;
      "fg+" = base06;
      "header" = base0D;
      "hl" = base0D;
      "hl+" = base0D;
      "info" = base0A;
      "marker" = base0C;
      "pointer" = base0C;
      "prompt" = base0A;
      "spinner" = base0C;
    });
in
{
  options.stylix.targets.fzf = {
    enable = config.lib.stylix.mkEnableTarget "Fzf" config.programs.fzf.enable;
  };

  config = lib.mkIf config.stylix.targets.fzf.enable {
    programs.fzf.defaultOptions = lib.mkAfter [ "--color=${colorConfig}" ];
  };
}
