{ config, ... }:

with config.lib.stylix.colors;
with config.stylix.fonts;

{
  stylix.homeModule = {
    services.dunst.settings = {
      global = {
        separator_color = base02-hash;
        font = sansSerif.name;
      };

      urgency_low = {
        background = base01-hash;
        foreground = base05-hash;
        frame_color = base0B-hash;
      };

      urgency_normal = {
        background = base01-hash;
        foreground = base05-hash;
        frame_color = base0E-hash;
      };

      urgency_critical = {
        background = base01-hash;
        foreground = base05-hash;
        frame_color = base08-hash;
      };
    };
  };
}
