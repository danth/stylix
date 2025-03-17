{
  config,
  lib,
  ...
}:
with config.lib.stylix.colors.withHashtag;
{
  options.stylix.targets.rstudio = {
    enable = config.lib.stylix.mkEnableTarget "RStudio" true;
  };

  config =
    lib.mkIf (config.stylix.enable && config.stylix.targets.rstudio.enable)
      {
        xdg.configFile."rstudio/theme/stylix.rtheme".text =
          with config.lib.stylix.colors.withHashtag;
          ''
              :root {
              --base00: ${base00}
              --base01: ${base01}
              --base02: ${base02}
              --base03: ${base03}
              --base04: ${base04}
              --base05: ${base05}
              --base06: ${base06}
              --base07: ${base07}
              --base08: ${base08}
              --base09: ${base09}
              --base0A: ${base0A}
              --base0B: ${base0B}
              --base0C: ${base0C}
              --base0D: ${base0D}
              --base0E: ${base0E}
              --base0F: ${base0F}
            }
          ''
          + (builtins.readFile ./base.rtheme);
      };
}
