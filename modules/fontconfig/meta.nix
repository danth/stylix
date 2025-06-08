{ lib, ... }:
{
  name = "Fontconfig";
  homepage = "https://fontconfig.org";
  maintainers = [ lib.maintainers.mightyiam ];
  description = ''
    On NixOS, `fonts.fontconfig.defaultFonts` options are declared without
    enabling `fonts.fontconfig.enable`. On Home Manager, only the
    `fonts.fontconfig.enable` option is declared and enabled.

    ### Related modules

    <!-- If updating this section, make sure to update it on the linked pages too. -->

    - [Font packages](font-packages.md)
  '';
}
