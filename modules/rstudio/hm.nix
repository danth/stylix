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
        xdg.configFile."rstudio/themes/stylix.rstheme".source =
          config.lib.stylix.colors
            {
              template = ./base.rstheme.mustache;
              extension = ".rstheme";
            };

      };
}
