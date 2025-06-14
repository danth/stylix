{ lib, ... }:
{
  name = "Font packages";
  maintainers = [ lib.maintainers.mightyiam ];
  description = ''
    This module makes the Stylix fonts available in the environment of each of
    the platforms that this module supports.

    Also see [general fonts documentation](../../configuration.html#fonts).
  '';
}
