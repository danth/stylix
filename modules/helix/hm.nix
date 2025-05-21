{
  mkTarget,
  pkgs,
  lib,
  ...
}:
mkTarget {
  name = "helix";
  humanName = "Helix";

  extraOptions.transparent = lib.mkEnableOption "transparent theming" // {
    internal = true;
    default = false;
  };

  configElements = [
    (
      { opacity }:
      {
        stylix.targets.helix.transparent = opacity.terminal != 1.0;
      }
    )
    (
      {
        cfg,
        colors,
        inputs,
      }:
      {
        programs.helix = {
          settings.theme = "stylix";

          themes.stylix =
            let
              theme = colors {
                templateRepo = inputs.base16-helix;
              };

              # Removing the background exposes transparency from the terminal. The
              # background might be helpful if the terminal isn't themed, so we only
              # do this if transparency is actually enabled.
              transparentTheme = pkgs.runCommandLocal "helix-transparent.toml" { } ''
                sed 's/,\? bg = "base00"//g' <${theme} >$out
              '';
            in
            if cfg.transparent then transparentTheme else theme;
        };
      }
    )
  ];
}
