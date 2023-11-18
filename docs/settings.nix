# Dummy wallpaper to avoid errors when generating the documentation.

{ pkgs, config, ... }:

{
  stylix.wallpaper = config.lib.stylix.make.static {
    image = pkgs.fetchurl {
      url = "https://picsum.photos/seed/stylix/1920/1080.jpg";
      sha256 = "tow7DBO+/tgkDDD8diif+0V/6aDzrYl02/J7SCA8RCU=";
    };
  };
}
