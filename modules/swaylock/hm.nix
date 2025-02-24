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
    enable =
      config.lib.stylix.mkEnableTarget "Swaylock"
        # When the state version is older than 23.05, Swaylock enables itself
        # automatically if `settings != {}` [1]. Therefore, Swaylock theming
        # shouldn't be enabled by default for such state versions, to avoid
        # inadvertently installing Swaylock when it's not desired.
        #
        # [1]: https://github.com/nix-community/home-manager/blob/5cfbf5cc37a3bd1da07ae84eea1b828909c4456b/modules/programs/swaylock.nix#L12-L17
        (lib.versionAtLeast config.home.stateVersion "23.05");

    useWallpaper = config.lib.stylix.mkEnableWallpaper "Swaylock" true;
  };

  config =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.swaylock.enable
        && pkgs.stdenv.hostPlatform.isLinux
        # Adding `&& config.programs.swaylock.enable` here may lead to infinite
        # recursion, due to the default value depending on `settings != {}`
        # when the state version is older than 23.05 [1], and the content of
        # this module affecting that default.
        #
        # [1]: https://github.com/nix-community/home-manager/blob/5cfbf5cc37a3bd1da07ae84eea1b828909c4456b/modules/programs/swaylock.nix#L12-L17
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
