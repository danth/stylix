{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.stylix.targets.wayfire;
in

{
  options.stylix.targets.wayfire = {
    enable = config.lib.stylix.mkEnableTarget "wayfire" true;
    useWallpaper = config.lib.stylix.mkEnableWallpaper "wayfire" true;
  };

  config = lib.mkIf (config.stylix.enable && cfg.enable) (
    let
      inherit (config.lib.stylix) colors;
      rgba = rgb: a: "\\#${rgb}${a}";
      rgb = (lib.flip rgba) "ff";

      wayfireConfig = config.wayland.windowManager.wayfire;

      wayfireBackground = lib.mkIf cfg.useWallpaper (
        pkgs.runCommand "wayfire-background.png" { } ''
          ${lib.getExe' pkgs.imagemagick "convert"} ${config.stylix.image} $out
        ''
      );
    in
    {
      wayland.windowManager.wayfire.settings = lib.mkIf wayfireConfig.enable {
        cube = {
          background = rgb colors.base00;
          cubemap_image = lib.mkIf cfg.useWallpaper "${wayfireBackground}";
          skydome_texture = lib.mkIf cfg.useWallpaper "${wayfireBackground}";
        };

        expo.background = rgb colors.base00;
        vswitch.background = rgb colors.base00;
        vswipe.background = rgb colors.base00;
        core.background_color = rgb colors.base00;

        decoration = {
          font = "${config.stylix.fonts.monospace.name} ${builtins.toString config.stylix.fonts.sizes.desktop}";
          active_color = rgb colors.base0D;
          inactive_color = rgb colors.base03;
        };
      };

      wayland.windowManager.wayfire.wf-shell.settings =
        lib.mkIf wayfireConfig.wf-shell.enable
          {
            background.image = lib.mkIf cfg.useWallpaper "${wayfireBackground}";
            background.fill_mode =
              if config.stylix.imageScalingMode == "stretch" then
                "stretch"
              else if config.stylix.imageScalingMode == "fit" then
                "preserve_aspect"
              else
                "fill_and_crop";

            panel.background_color = rgb colors.base01;
            panel.menu_icon = "${pkgs.nixos-icons}/share/icons/hicolor/256x256/apps/nix-snowflake.png";
          };
    }
  );
}
