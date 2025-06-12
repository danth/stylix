{
  mkTarget,
  pkgs,
  config,
  lib,
  ...
}:
mkTarget {
  name = "swaylock";
  humanName = "Swaylock";

  # When the state version is older than 23.05, Swaylock enables itself
  # automatically if `settings != {}` [1]. Therefore, Swaylock theming
  # shouldn't be enabled by default for such state versions, to avoid
  # inadvertently installing Swaylock when it's not desired.
  #
  # Adding `&& config.programs.swaylock.enable` below may lead to infinite
  # recursion, due to the above.
  #
  # [1]: https://github.com/nix-community/home-manager/blob/5cfbf5cc37a3bd1da07ae84eea1b828909c4456b/modules/programs/swaylock.nix#L12-L17
  autoEnable =
    lib.versionAtLeast config.home.stateVersion "23.05"
    && pkgs.stdenv.hostPlatform.isLinux;

  autoEnableExpr = ''
    lib.versionAtLeast home.stateVersion "23.05" && pkgs.stdenv.hostPlatform.isLinux
  '';

  extraOptions.useWallpaper = config.lib.stylix.mkEnableWallpaper "Swaylock" true;

  configElements = [
    (
      { colors }:
      {
        programs.swaylock.settings =
          with colors;
          let
            inside = base00-hex;
            outside = base00-hex;
            ring = base01-hex;
            text = base05-hex;
            positive = base0B-hex;
            negative = base08-hex;
          in
          {
            color = outside;
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
          };
      }
    )
    (
      { cfg, image }:
      {
        programs.swaylock.settings.image = lib.mkIf cfg.useWallpaper "${image}";
      }
    )
    (
      { imageScalingMode }:
      {
        programs.swaylock.settings.scaling = imageScalingMode;
      }
    )
  ];
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
}
