{ lib, ... }:
{
  name = "i3status-rust";
  homepage = "https://github.com/greshake/i3status-rust";
  maintainers = [ lib.maintainers.mightyiam ];
  description = ''
    Theming for bars is not automatically applied. See
    [this option](#stylixtargetsi3status-rustexportedbarconfig), which provides
    the exported configuration.
  '';
}
