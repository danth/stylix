{ config, lib, ... }:

{
  options.stylix.targets.zellij.enable =
    config.lib.stylix.mkEnableTarget "zellij" true;

  config =
    lib.mkIf
      (
        config.stylix.enable
        && config.stylix.targets.zellij.enable
        && config.programs.zellij.enable
      )
      {
        xdg.configFile."zellij/themes/stylix.kdl".text =
          with config.lib.stylix.colors.withHashtag; ''
            themes {
              default {
                bg "${base03}";
                fg "${base05}";
                red "${base01}";
                green "${base0B}";
                blue "${base0D}";
                yellow "${base0A}";
                magenta "${base0E}";
                orange "${base09}";
                cyan "${base0C}";
                black "${base00}";
                white "${base07}";
              }
            }
          '';
      };

}
