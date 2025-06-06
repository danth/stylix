{ lib, ... }:
{
  name = "Fontconfig";
  homepage = "https://fontconfig.org";
  maintainers = [ lib.maintainers.mightyiam ];
  description = ''
    This module adds the Stylix fonts to `fonts.fontconfig.defaultFonts` in
    each of the platforms that this module supports.

    For that to take effect, make sure `fonts.fontconfig.enable` is `true`.

    Also see [general fonts documentation](/configuration.html#fonts).
  '';
}
