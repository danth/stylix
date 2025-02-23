{
  config,
  lib,
  pkgs,
  ...
}:

let
  mkFont =
    font:
    pkgs.runCommand "stylix-font-${font.package.name}"
      {
        FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ font.package ]; };
      }
      ''
        font="$(
          ${lib.getExe' pkgs.fontconfig "fc-match"} \
          ${lib.escapeShellArg font.name} \
          --format %{file}
        )"

        cp "$font" "$out"
      '';
  terminalFont = mkFont config.stylix.fonts.monospace;
in
{
  imports = [ ../fonts.nix ];

  config.terminal.font = lib.mkIf config.stylix.enable terminalFont;
}
