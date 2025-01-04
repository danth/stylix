{ config, lib, ... }:

{
  options.stylix.targets.chromium.enable =
    config.lib.stylix.mkEnableTarget "Chromium, Google Chrome and Brave" true;

  config.programs.chromium =
    lib.mkIf (config.stylix.enable && config.stylix.targets.chromium.enable)
      {
        # This enables policies without installing the browser. Policies take up a
        # negligible amount of space, so it's reasonable to have this always on.
        enable = true;

        extraOpts.BrowserThemeColor = config.lib.stylix.colors.withHashtag.base00;
      };
}
