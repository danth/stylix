# Used to build templates from http://chriskempson.com/projects/base16/

{ pkgs, config, ... }:

{
  config.lib.stylix.base16 = rec {
    json = builtins.toJSON (config.lib.stylix.colors // {
      # Additional attributes only used for base16
      scheme-name = "Stylix";
      scheme-slug = "stylix";
      scheme-author = "Stylix";
    });

    buildTemplate = name: templatePath:
      pkgs.runCommand "base16-${name}"
      {
        inherit json templatePath;
        passAsFile = [ "json" ];
      }
      "${pkgs.mustache-go}/bin/mustache $jsonPath $templatePath > $out";
  };
}
