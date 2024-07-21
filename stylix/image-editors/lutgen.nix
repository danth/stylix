{
  lib,
  config,
  pkgs,
  ...
}:
img:
let
  colors = lib.strings.concatStringsSep " " config.lib.stylix.colors.toList;
  baseName = builtins.baseNameOf img;
in
pkgs.runCommand baseName { } ''
  ${pkgs.lutgen}/bin/lutgen apply '${img}' -o $out -- ${colors}
''
