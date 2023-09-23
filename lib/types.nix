{ base16 }:
{ config, lib, options, ... }@args:
with lib;
with config.lib.stylix;
{
  config.lib.stylix = {
    static = mkOptionType {
      name = "static";
      description = "static wallpaper";
      descriptionClass = "noun";
      check = isStatic;
    };
    isStatic = value:
      builtins.attrNames value == [ "colors" "image" ];

    animation = mkOptionType {
      name = "animation";
      description = "animated wallpaper";
      descriptionClass = "noun";
      check = isAnimation;
    };
    isAnimation = value:
      builtins.attrNames value == [ "animation" "colors" "image" ];

    video = mkOptionType {
      name = "video";
      description = "video wallpaper";
      descriptionClass = "noun";
      check = isVideo;
    };
    isVideo = value:
      builtins.attrNames value == [ "colors" "image" "video" ];

    slideshow = mkOptionType {
      name = "slideshow";
      description = "slideshow wallpaper";
      descriptionClass = "noun";
      check = isSlideshow;
    };
    isSlideshow = value:
      builtins.attrNames value == [ "colors" "delay" "image" "images" ];

    scheme = mkOptionType {
      name = "scheme";
      description = "base16 scheme";
      descriptionClass = "noun";
      check = isScheme;
    };
    isScheme = value:
      let
        bases = [
          "base00" "base01" "base02" "base03"
          "base04" "base05" "base06" "base07"
          "base08" "base09" "base0A" "base0B"
          "base0C" "base0D" "base0E" "base0F"
        ];
        meta = [
          "scheme" "author" "description" "slug"
        ];
        requiredAttrs = bases;
        allowedAttrs = bases ++ meta;
      in
        types.path.check value ||
        (builtins.isAttrs value
          && all (a: elem a allowedAttrs) (builtins.attrNames value)
          && all (a: elem a (builtins.attrNames value)) requiredAttrs
        );

    override = mkOptionType {
      name = "override";
      description = "override for base16 scheme";
      descriptionClass = "noun";
      emptyValue.value = { };
      check = isOverride;
    };
    isOverride = value:
      let allowedAttrs = [
        "base00" "base01" "base02" "base03"
        "base04" "base05" "base06" "base07"
        "base08" "base09" "base0A" "base0B"
        "base0C" "base0D" "base0E" "base0F"
        "scheme" "author" "description" "slug"
      ];
      in builtins.isAttrs value
        && all (a: elem a allowedAttrs) (builtins.attrNames value);

    overridableScheme = mkOptionType {
      name = "overriableScheme";
      description = "base16 scheme with overrides";
      descriptionClass = "conjunction";
      emptyValue.value = { };
      check = value: isScheme value || isOverride value;
      merge = optionName: definitions:
        let
          values = catAttrs "value" definitions;
          partitioned = partition isScheme values;
          scheme =
            if length partitioned.right < 1
            then
              # If only overrides were provided, try applying them to the option default.
              if hasAttrByPath (optionName ++ [ "default" ]) options
              then getAttrFromPath (optionName ++ [ "default" ]) options
              else throw "At least one definition for `${showOption optionName}' must be a whole scheme."
            else
              if length partitioned.right > 1
              then throw "Only one definition for `${showOption optionName}' may be a whole scheme."
              else elemAt partitioned.right 0;
        in foldr
          (o: s: s.override o)
          (base16.mkSchemeAttrs scheme)
          partitioned.wrong;
    };
  };
}
