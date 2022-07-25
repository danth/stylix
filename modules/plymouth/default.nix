{ config, pkgs, lib, ... }:

with config.lib.stylix.colors;

let
  theme = pkgs.runCommandLocal "plymouth-theme" { } ''
    themeDir="$out/share/plymouth/themes/stylix"
    mkdir -p $themeDir

    cp ${./theme.script} $themeDir/stylix.script

    # Convert in case the input image is not PNG
    ${pkgs.imagemagick}/bin/convert ${config.stylix.image} $themeDir/background.png

    cp ${config.lib.stylix.pixel "base0B"} $themeDir/progress.png

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
  options.stylix.targets.plymouth.enable =
    config.lib.stylix.mkEnableTarget "the Plymouth boot screen" true;

  config.boot.plymouth = lib.mkIf config.stylix.targets.plymouth.enable {
    theme = "stylix";
    themePackages = [ theme ];
  };
}
