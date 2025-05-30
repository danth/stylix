{ lib, ... }:
{
  name = "Fontconfig";
  homepage = "https://fontconfig.org";
  maintainers = [ lib.maintainers.mightyiam ];
  description = ''
    On NixOS:

    - Does _not_ enable Fontconfig.
    - Adds the Stylix fonts to the corresponding `fonts.fontconfig.defaultFonts` options.

    On home-manager:

    - Enables Fontconfig.

    ### Related modules

    <!-- If updating this section, make sure to update it on the linked pages too. -->

    - [Font packages](font-packages.md)
  '';
}
