{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.stylix.targets.steam;

  adwaitaCustomCss = config.lib.stylix.colors {
    template = ./custom.mustache;
    extension = "css";
  };

  adwaitaTheme = pkgs.stdenv.mkDerivation (_self: {
    name = "Adwaita-for-Steam";
    version = "3.1";

    src = config.stylix.inputs.adwaita-for-steam;

    buildInputs = with pkgs; [ python3 ];

    buildPhase = ''
      mkdir --parents $out/steamui/css
      touch $out/steamui/css/library.css
      cp ${adwaitaCustomCss} ./custom/custom.css
      python3 install.py --custom-css --target $out
    '';
  });
in
{
  options.stylix.targets.steam = {
    enable = config.lib.stylix.mkEnableTarget "Steam" true;
    adwaitaForSteam.enable = config.lib.stylix.mkEnableTarget "Adwaita for Steam" true;
  };

  config =
    lib.mkIf (config.stylix.enable && cfg.enable && cfg.adwaitaForSteam.enable)
      {
        home.activation.backupSteamCss =
          let
            shellScript = pkgs.writeShellScript "backup-steam-css" ''
              css_dir="${config.xdg.dataHome}/Steam/steamui/css"
              cp --no-clobber "$css_dir/library.css" "$css_dir/library.original.css"
            '';
          in
          config.lib.dag.entryBefore [ "writeBoundary" ] ''
            run --quiet ${shellScript}
          '';

        xdg.dataFile = {
          "Steam/steamui/adwaita".source = "${adwaitaTheme}/steamui/adwaita";
          "Steam/steamui/libraryroot.custom.css".source =
            "${adwaitaTheme}/steamui/libraryroot.custom.css";
          "Steam/steamui/css/library.css".source =
            "${adwaitaTheme}/steamui/css/library.css";
        };
      };
}
