{ mkTarget, ... }:
mkTarget {
  name = "avizo";
  humanName = "Avizo";

  # Referenced https://github.com/stacyharper/base16-mako
  configElements =
    { colors, opacity }:
    {
      services.avizo = {
        settings = {
          default =
            with colors;
            let
              aviOpacity = toString opacity.popups;
            in
            {
              background = "rgba(${base01-rgb-r}, ${base01-rgb-g}, ${base01-rgb-b}, ${aviOpacity})";
              border-color = "rgba(${base0D-rgb-r}, ${base0D-rgb-g}, ${base0D-rgb-b}, ${aviOpacity})";
              bar-fg-color = "rgba(${base05-rgb-r}, ${base05-rgb-g}, ${base05-rgb-b}, ${aviOpacity})";
              bar-bg-color = "rgba(${base01-rgb-r}, ${base01-rgb-g}, ${base01-rgb-b}, ${aviOpacity})";
              image-opacity = aviOpacity;
            };
        };
      };
    };
}
