{
  config,
  lib,
  ...
}:

with config.lib.stylix.colors.withHashtag;
with config.stylix.fonts;

{
  options.stylix.targets.swaync = {
    enable = config.lib.stylix.mkEnableTarget "SwayNC" true;
  };

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.swaync.enable)
      {
        services.swaync.style =
          ''
            @define-color base00 ${base00}; @define-color base01 ${base01}; @define-color base02 ${base02}; @define-color base03 ${base03};
            @define-color base04 ${base04}; @define-color base05 ${base05}; @define-color base06 ${base06}; @define-color base07 ${base07};

            @define-color base08 ${base08}; @define-color base09 ${base09}; @define-color base0A ${base0A}; @define-color base0B ${base0B};
            @define-color base0C ${base0C}; @define-color base0D ${base0D}; @define-color base0E ${base0E}; @define-color base0F ${base0F};

            * {
                font-family: "${sansSerif.name}";
                font-size: ${builtins.toString sizes.desktop}pt;
            }
          ''
          + (builtins.readFile ./base.css);
      };
}
