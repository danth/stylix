{
  config,
  lib,
  ...
}:
{
  options.stylix.targets.rstudio = {
    enable = config.lib.stylix.mkEnableTarget "RStudio" false;
  };

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.rstudio.enable)
      {
        xdg.configFile."rstudio/themes/stylix.rstheme.generated".source =
          config.lib.stylix.colors
            {
              template = ./base.rstheme.mustache;
              extension = ".rstheme.generated";
            };
        onChange = ''
          rm -f ${config.xdg.configHome}/rstudio/themes/stylix.theme
          cp ${config.xdg.configHome}rstudio/themes/stylix.rstheme.generated ${config.xdg.configHome}/rstudio/themes/stylix.theme
          chmod u+w ${config.xdg.configHome}/rstudio/themes/stylix.theme
        '';
      };
}
