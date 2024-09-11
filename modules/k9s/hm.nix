{ config, lib, ... }:

with config.lib.stylix.colors.withHashtag;

{
  options.stylix.targets.k9s.enable = config.lib.stylix.mkEnableTarget "k9s" true;

  config = lib.mkIf config.stylix.targets.k9s.enable {
    programs.k9s.skins.skin = {
      k9s = {
        body = {
          fgColor = base05-hex;
          bgColor = "default";
          logoColor = base0C-hex;
        };

        prompt = {
          fgColor = base05-hex;
          bgColor = base00-hex;
          suggestColor = base0A-hex;
        };

        info = {
          fgColor = base0B-hex;
          sectionColor = base05-hex;
        };

        dialog = {
          fgColor = base05-hex;
          bgColor = "default";
          buttonFgColor = base05-hex;
          buttonBgColor = base0C-hex;
          buttonFocusFgColor = base0E-hex;
          buttonFocusBgColor = base0B-hex;
          labelFgColor = base0A-hex;
          fieldFgColor = base05-hex;
        };

        frame = {
          border = {
            fgColor = base02-hex;
            focusColor = base01-hex;
          };

          menu = {
            fgColor = base05-hex;
            keyColor = base0B-hex;
            numKeyColor = base0B-hex;
          };

          crumbs = {
            fgColor = base05-hex;
            bgColor = base01-hex;
            activeColor = base01-hex;
          };

          status = {
            newColor = base08-hex;
            modifyColor = base0C-hex;
            addColor = base09-hex;
            errorColor = base0D-hex;
            highlightcolor = base0A-hex;
            killColor = base03-hex;
            completedColor = base03-hex;
          };

          title = {
            fgColor = base05-hex;
            bgColor = base01-hex;
            highlightColor = base0A-hex;
            counterColor = base0C-hex;
            filterColor = base0B-hex;
          };
        };

        views = {
          charts = {
            bgColor = "default";
            defaultDialColors = [
              base0C-hex
              base0D-hex
            ];
            defaultChartColors = [
              base0C-hex
              base0D-hex
            ];
          };

          table = {
            fgColor = base05-hex;
            bgColor = "default";
            header = {
              fgColor = base05-hex;
              bgColor = "default";
              sorterColor = base08-hex;
            };
          };

          xray = {
            fgColor = base05-hex;
            bgColor = "default";
            cursorColor = base01-hex;
            graphicColor = base0C-hex;
            showIcons = false;
          };

          yaml = {
            keyColor = base0B-hex;
            colonColor = base0C-hex;
            valueColor = base05-hex;
          };

          logs = {
            fgColor = base05-hex;
            bgColor = "default";
            indicator = {
              fgColor = base05-hex;
              bgColor = base0C-hex;
            };
          };

          help = {
            fgColor = base05-hex;
            bgColor = base00-hex;
            indicator.fgColor = base0D-hex;
          };
        };
      };
    };
  };
}
