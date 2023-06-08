{ config, lib, pkgs, ... }:

with config.lib.stylix.colors.withHashtag;

let
  text = base05;
  urgent = base08;
  focused = base0A;
  unfocused = base03;

  fonts = {
    names = [ config.stylix.fonts.sansSerif.name ];
    size = config.stylix.fonts.sizes.desktop + 0.0;
  };

in {
  options.stylix.targets.sway.enable =
    config.lib.stylix.mkEnableTarget "Sway" true;

  config = lib.mkMerge [
    (lib.mkIf config.stylix.targets.sway.enable {
      wayland.windowManager.sway.config = {
        inherit fonts;
          startup =
          let
            slideshowScript = pkgs.writeScript "script.sh" ''
            if [[ $# -lt 1 ]] || [[ ! -d $1 ]]; then
              echo "Usage:
              $0 <dir containg images>"
              exit 1
            fi

            export SWWW_TRANSITION_FPS=60
            export SWWW_TRANSITION_STEP=2

            # This controls (in seconds) when to switch to the next image
            INTERVAL=${builtins.toString config.stylix.wallpaper.delay}
            
            while true; do
            	find "$1" \
            		| while read -r img; do
            			echo "$((RANDOM % 1000)):$img"
            		done \
            		| sort -n | cut -d':' -f2- \
            		| while read -r img; do
            			${pkgs.swww}/bin/swww img "$img"
            			sleep $INTERVAL
            		done
            done
            '';
          in if (config.lib.stylix.isAnimation config.stylix.wallpaper) then [
              { command = "${pkgs.swww}/bin/swww-daemon"; }
              { command = "${pkgs.swww}/bin/swww img ${config.stylix.wallpaper.animation}"; }
          ] else if (config.lib.stylix.isSlideshow config.stylix.wallpaper) then [
              { command = "${pkgs.swww}/bin/swww-daemon"; }
              { command = "${slideshowScript} ${config.stylix.wallpaper.imageDir}"; }
          ] else if (config.lib.stylix.isVideo config.stylix.wallpaper) then [
              { command = "${pkgs.mpvpaper}/bin/mpvpaper '*' -o 'no-audio --loop' ${config.stylix.wallpaper.video}"; }
          ] else [
              { command = "${pkgs.wbg}/bin/wbg ${config.stylix.wallpaper.image}"; }
          ];
        colors = let
          background = base00;
          indicator = base0B;
        in {
          inherit background;
          urgent = {
            inherit background indicator text;
            border = urgent;
            childBorder = urgent;
          };
          focused = {
            inherit background indicator text;
            border = focused;
            childBorder = focused;
          };
          focusedInactive = {
            inherit background indicator text;
            border = unfocused;
            childBorder = unfocused;
          };
          unfocused = {
            inherit background indicator text;
            border = unfocused;
            childBorder = unfocused;
          };
          placeholder = {
            inherit background indicator text;
            border = unfocused;
            childBorder = unfocused;
          };
        };
      };
    })

    {
      # Merge this with your bar configuration using //config.lib.stylix.sway.bar
      lib.stylix.sway.bar = {
        inherit fonts;

        colors = let
          background = base01;
          border = background;
        in {
          inherit background;
          statusline = text;
          separator = base03;
          focusedWorkspace = {
            inherit text border;
            background = focused;
          };
          activeWorkspace = {
            inherit text border;
            background = unfocused;
          };
          inactiveWorkspace = {
            inherit text border;
            background = unfocused;
          };
          urgentWorkspace = {
            inherit text border;
            background = urgent;
          };
          bindingMode = {
            inherit text border;
            background = urgent;
          };
        };
      };
    }
  ];
}
