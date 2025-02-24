{
  pkgs,
  config,
  lib,
  ...
}:

with config.lib.stylix.colors;

let
  cfg = config.stylix.targets.swaylock;

  inside = base01-hex;
  outside = base01-hex;
  ring = base05-hex;
  text = base05-hex;
  positive = base0B-hex;
  negative = base08-hex;

in
{
  imports = [
    (lib.mkRenamedOptionModuleWith {
      from = [
        "stylix"
        "targets"
        "swaylock"
        "useImage"
      ];
      sinceRelease = 2505;
      to = [
        "stylix"
        "targets"
        "swaylock"
        "useWallpaper"
      ];
    })
  ];
  options.stylix.targets.swaylock = {
    enable = config.lib.stylix.mkEnableTarget "Swaylock" true;
    useWallpaper = config.lib.stylix.mkEnableWallpaper "Swaylock" true;
  };

  config =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.swaylock.enable
        && pkgs.stdenv.hostPlatform.isLinux

        # Avoid inadvertently installing the Swaylock package by preventing the
        # Home Manager module from enabling itself when 'settings != {}' and the
        # state version is older than 23.05 [1].
        #
        # [1]: https://github.com/nix-community/home-manager/blob/5cfbf5cc37a3bd1da07ae84eea1b828909c4456b/modules/programs/swaylock.nix#L12-L17
        && config.programs.swaylock.enable
      )
      {
        programs.swaylock.settings =
          {
            color = outside;
            scaling = config.stylix.imageScalingMode;
            inside-color = inside;
            inside-clear-color = inside;
            inside-caps-lock-color = inside;
            inside-ver-color = inside;
            inside-wrong-color = inside;
            key-hl-color = positive;
            layout-bg-color = inside;
            layout-border-color = ring;
            layout-text-color = text;
            line-uses-inside = true;
            ring-color = ring;
            ring-clear-color = negative;
            ring-caps-lock-color = ring;
            ring-ver-color = positive;
            ring-wrong-color = negative;
            separator-color = "00000000";
            text-color = text;
            text-clear-color = text;
            text-caps-lock-color = text;
            text-ver-color = text;
            text-wrong-color = text;
          }
          // lib.optionalAttrs cfg.useWallpaper {
            image = "${config.stylix.image}";
          };
      };
}
