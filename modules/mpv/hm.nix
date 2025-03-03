{ lib, config, ... }:
{
  options.stylix.targets.mpv.enable = config.lib.stylix.mkEnableTarget "Mpv" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.mpv.enable) {
    programs.mpv = {
      config = with config.lib.stylix.colors.withHashtag; {
        background-color = base00;
        osd-back-color = base01;
        osd-border-color = base01;
        osd-color = base05;
        osd-shadow-color = base00;
      };

      scriptOpts.uosc = with config.lib.stylix.colors; {
        background = base00;
        background_text = base05;
        curtain = base0D;
        error = base0F;
        foreground = base05;
        foreground_text = base00;
        success = base0A;
      };
    };
  };
}
