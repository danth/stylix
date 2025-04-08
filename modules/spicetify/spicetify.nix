{
  config,
  options,
  lib,
  pkgs,
  ...
}:

{
  options.stylix.targets.spicetify.enable =
    config.lib.stylix.mkEnableTarget "Spicetify" true;

  config =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.spicetify.enable
        && (config.programs ? spicetify)
      )
      (
        lib.optionalAttrs (builtins.hasAttr "spicetify" options.programs) {
          programs.spicetify = {
            theme = {
              name = "stylix";
              src = pkgs.writeTextFile {
                name = "color.ini";
                destination = "/color.ini";
                text = with config.lib.stylix.colors; ''
                  [base]
                  text               = ${base05}
                  subtext            = ${base05}
                  main               = ${base00}
                  main-elevated      = ${base02}
                  highlight          = ${base02}
                  highlight-elevated = ${base03}
                  sidebar            = ${base01}
                  player             = ${base05}
                  card               = ${base04}
                  shadow             = ${base00}
                  selected-row       = ${base05}
                  button             = ${base05}
                  button-active      = ${base05}
                  button-disabled    = ${base04}
                  tab-active         = ${base02}
                  notification       = ${base02}
                  notification-error = ${base08}
                  equalizer          = ${base0B}
                  misc               = ${base02}
                '';
              };
              # Sidebar configuration is incompatible with the default navigation bar
              sidebarConfig = false;
            };
            colorScheme = "base";
          };
        }
      );
}
