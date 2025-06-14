{ lib, config, ... }:
let
  opacityHex = lib.toHexString (
    builtins.ceil (config.stylix.opacity.desktop * 255)
  );
in
{
  options.stylix.targets.i3status-rust.exportedBarConfig = lib.mkOption {
    type = lib.types.attrs;
    description = ''
      Theming configuration which can be merged with your own:
      ```nix
      programs.i3status-rust.bars.«name».settings.theme.overrides =
        {
          # your configuration
        }
        // config.stylix.targets.i3status-rust.exportedBarConfig;
      ```
    '';
    readOnly = true;
  };
  config.stylix.targets.i3status-rust.exportedBarConfig =
    lib.mapAttrs (n: v: if lib.hasSuffix "_bg" n then v + opacityHex else v)
      (
        with config.lib.stylix.colors.withHashtag;
        {
          idle_bg = base00;
          idle_fg = base05;
          info_bg = base09;
          info_fg = base00;
          good_bg = base01;
          good_fg = base05;
          warning_bg = base0A;
          warning_fg = base00;
          critical_bg = base08;
          critical_fg = base00;
          separator_bg = base00;
          separator_fg = base05;
        }
      );
}
