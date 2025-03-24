{
  lib,
  pkgs,
  config,
  ...
}:
{
  options.stylix.targets.fnott.enable =
    config.lib.stylix.mkEnableTarget "Fnott" true;

  config.xdg.configFile."fnott/fnott.ini".source =
    with config.lib.stylix.colors;
    with config.stylix.fonts;
    let
      fg = color: color + "ff";
      bg =
        color:
        color
        + lib.toHexString (
          ((builtins.ceil (config.stylix.opacity.popups * 100 + 0.5)) * 255) / 100
        );
      font = config.stylix.fonts.sansSerif.name;

      # this is a hacky workaround for https://codeberg.org/dnkl/fnott/issues/137
      fnott-settings = {
        globalSection = {
          title-font = "${font}:size=${toString sizes.popups}";
          summary-font = "${font}:size=${toString sizes.popups}";
          body-font = "${font}:size=${toString sizes.popups}";

          icon-theme = config.gtk.iconTheme.name;

          title-color = fg base05;
          summary-color = fg base05;
          body-color = fg base05;
          progress-bar-color = fg base02;
          border-color = fg base0D;
          background = bg base00;
        };

        sections = {
          low.border-color = fg base03;
          critical.border-color = fg base08;
        };
      };
    in
    lib.mkForce (
      pkgs.writeText "fnott.ini" (
        lib.generators.toINIWithGlobalSection { } fnott-settings
      )
    );
}
