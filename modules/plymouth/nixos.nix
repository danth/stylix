{ config, pkgs, lib, ... }:

with lib;
with config.lib.stylix.colors;

let
  cfg = config.stylix.targets.plymouth;

  theme = pkgs.runCommand "stylix-plymouth" { } ''
    themeDir="$out/share/plymouth/themes/stylix"
    mkdir -p $themeDir

    ${pkgs.imagemagick}/bin/convert \
      -background transparent \
      -bordercolor transparent \
      ${
        # A transparent border ensures the image is not clipped when rotated
        optionalString cfg.logoAnimated "-border 42%"
      } \
      ${cfg.logo} \
      $themeDir/logo.png

    ${
      if cfg.logoAnimated
      then "cp ${./theme.script} $themeDir/stylix.script"
      else "cp ${./theme_still.script} $themeDir/stylix.script"
    }

    substituteInPlace $themeDir/stylix.script \
      --replace-fail "%BASE00%" "${base00-dec-r}, ${base00-dec-g}, ${base00-dec-b}" \
      --replace-fail "%BASE05%" "${base05-dec-r}, ${base05-dec-g}, ${base05-dec-b}"

    echo "
    [Plymouth Theme]
    Name=Stylix
    ModuleName=script

    [script]
    ImageDir=$themeDir
    ScriptFile=$themeDir/stylix.script
    " > $themeDir/stylix.plymouth
  '';

in {
  options.stylix.targets.plymouth = {
    enable = config.lib.stylix.mkEnableTarget "the Plymouth boot screen" true;

    logo = mkOption {
      description = mdDoc "Logo to be used on the boot screen.";
      type = with types; either path package;
      defaultText = literalMD "NixOS logo";

      # Considering that Flake inputs are currently unable to fetch individual
      # files, the SVG file is fetched with `pkgs.fetchurl` to avoid downloading
      # the entire repository for a single SVG file.
      default = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/NixOS/nixos-artwork/f84c13adae08e860a7c3f76ab3a9bef916d276cc/logo/nix-snowflake-colours.svg";
        # Reduce size
        postFetch = ''
          substituteInPlace $out \
            --replace-fail "141.5919" "70.79595" \
            --replace-fail "122.80626" "61.40313"
        '';
        sha256 = "4+MWdqESKo9omd3q0WfRmnrd3Wpe2feiayMnQlA4izU=";
      };
    };

    logoAnimated = mkOption {
      description = mdDoc ''
        Whether to apply a spinning animation to the logo.

        Disabling this allows the use of logos which don't have rotational
        symmetry.
      '';
      type = types.bool;
      default = true;
    };
  };

  imports = [
    (
      lib.mkRemovedOptionModule
      [ "stylix" "targets" "plymouth" "blackBackground" ]
      "This was removed since it goes against the chosen color scheme. If you want this, consider disabling the target and configuring Plymouth by hand."
    )
  ];

  config.boot.plymouth = mkIf cfg.enable {
    theme = "stylix";
    themePackages = [ theme ];
  };
}
