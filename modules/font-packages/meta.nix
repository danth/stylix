{ lib, ... }:
{
  name = "Font packages";
  maintainers = [ lib.maintainers.mightyiam ];
  description = ''
    This module makes the Stylix fonts available in the environment of each of
    the platforms that this module supports.

    ### Related modules

    <!-- If updating this section, make sure to update it on the linked pages too. -->

    - [Fontconfig](fontconfig.md)
  '';
}
