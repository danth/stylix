{ config, ... }:

with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;

{
  home-manager.sharedModules = [{
    services.dunst.settings = {
      global = {
        separator_color = base02;
        font = sansSerif.name;
      };

      urgency_low = {
        background = base01;
        foreground = base05;
        frame_color = base0B;
      };

      urgency_normal = {
        background = base01;
        foreground = base05;
        frame_color = base0E;
      };

      urgency_critical = {
        background = base01;
        foreground = base05;
        frame_color = base08;
      };
    };
  }];
}
