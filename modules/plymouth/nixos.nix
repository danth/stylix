{
  config,
  pkgs,
  lib,
  ...
}:

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
        lib.optionalString cfg.logoAnimated "-border 42%"
      } \
      ${cfg.logo} \
      $themeDir/logo.png

    ${
      if cfg.logoAnimated then
        "cp ${./theme.script} $themeDir/stylix.script"
      else
        "cp ${./theme_still.script} $themeDir/stylix.script"
    }

    ${with config.lib.stylix.colors; ''
      substituteInPlace $themeDir/stylix.script \
        --replace-fail "%BASE00%" "${base00-dec-r}, ${base00-dec-g}, ${base00-dec-b}" \
        --replace-fail "%BASE05%" "${base05-dec-r}, ${base05-dec-g}, ${base05-dec-b}"
    ''}

    echo "
    [Plymouth Theme]
    Name=Stylix
    ModuleName=script

    [script]
    ImageDir=$themeDir
    ScriptFile=$themeDir/stylix.script
    " > $themeDir/stylix.plymouth
  '';

in
{
  options.stylix.targets.plymouth = {
    enable = config.lib.stylix.mkEnableTarget "the Plymouth boot screen" true;

    logo = lib.mkOption {
      description = "Logo to be used on the boot screen.";
      type = with lib.types; either path package;
      defaultText = lib.literalMD "NixOS logo";

      default = "${pkgs.nixos-icons}/share/icons/hicolor/256x256/apps/nix-snowflake.png";
    };

    logoAnimated = lib.mkOption {
      description = ''
        Whether to apply a spinning animation to the logo.

        Disabling this allows the use of logos which don't have rotational
        symmetry.
      '';
      type = lib.types.bool;
      default = true;
    };
  };

  imports = [
    (lib.mkRemovedOptionModule [ "stylix" "targets" "plymouth" "blackBackground" ]
      "This was removed since it goes against the chosen color scheme. If you want this, consider disabling the target and configuring Plymouth by hand."
    )
  ];

  config.boot.plymouth = lib.mkIf cfg.enable {
    theme = "stylix";
    themePackages = [ theme ];
  };
}
