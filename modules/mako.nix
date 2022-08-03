{pkgs, config, lib, ... }:

with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;
{
  # Referenced https://github.com/stacyharper/base16-mako
  home-manager.sharedModules = [{
    programs.mako = {
      backgroundColor = base00;
      borderColor = base0D;
      textColor = base05;
      progressColor = "over ${base02}";
      font = sansSerif.name;
      # I wish the mako hm module was like the dunst one
      extraConfig = ''
        [urgency=low]
        background-color=${base00}
        border-color=${base0D}
        text-color=${base0A}

        [urgency=high]
        background-color=${base00}
        border-color=${base0D}
        text-color=${base08}
      '';
    }; 
  }];
}
