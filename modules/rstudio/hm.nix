{
  pkgs,
  config,
  lib,
  ...
}:
{
  options.stylix.targets.rstudio.enable =
    config.lib.stylix.mkEnableTarget "RStudio" true;

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.rstudio.enable)
      {
        xdg.configFile."rstudio/themes/stylix.rstheme" = {
          source = config.lib.stylix.colors {
            template = ./base.rstheme.mustache;
            extension = ".rstheme";
          };
        };
        # Modifies the RStudio config file to change the theme to stylix
        home.activation.rstudioThemeSelect =
          lib.hm.dag.entryAfter [ "writeBoundary" ]
            ''
              CONF=$HOME/.config/rstudio/rstudio-prefs.json
              if [ -f "$CONF" ]; then
                ${pkgs.jq}/bin/jq -r '.editor_theme|= "stylixBase16"' "$CONF" | ${pkgs.moreutils}/bin/sponge "$CONF"
              else
                echo "{\"editor_theme\": \"stylixBase16\"}" >"$CONF"
              fi
            '';
      };
}
