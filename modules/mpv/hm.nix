{ lib, config, ... }:
{
  options.stylix.targets.mpv.enable = config.lib.stylix.mkEnableTarget "mpv" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.mpv.enable) {
    programs.mpv = {
      config = with config.lib.stylix.colors.withHashtag; {
        osd-font = config.stylix.fonts.monospace.name;
        sub-font = config.stylix.fonts.monospace.name;
        osd-font-size = config.stylix.fonts.sizes.applications;
        sub-font-size = config.stylix.fonts.sizes.applications;

        background-color = base00;
        osd-back-color = base01;
        osd-border-color = base01;
        osd-color = base05;
        osd-shadow-color = base00;
      };

      scriptOpts.uosc.color =
        with config.lib.stylix.colors;
        lib.concatMapAttrsStringSep "," (name: value: "${name}=${value}") {
          background = base00;
          background_text = base05;
          foreground = base05;
          foreground_text = base00;
          curtain = base0D;
          success = base0A;
          error = base0F;
        };
    };
  };
}
