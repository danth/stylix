{ lib, ... }:
{
  maintainers = [ lib.maintainers.make-42 ];
  name = "Blender";
  homepage = "https://www.blender.org/";
  description = ''
    > [!IMPORTANT]
    >
    > This target will have no effect unless the Blender theme is properly
    > [enabled](
    > https://docs.blender.org/manual/en/latest/editors/preferences/themes.html)
    > within Blender itself.
  '';
}
