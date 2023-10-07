{ pkgs, config, lib, ... }:

let
  cfg = config.stylix.targets.foot;

  theme = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
      owner = "tinted-theming";
      repo = "base16-foot";
      rev = "22579065fbe21361b64d80bae798666efaa85ce0";
      sha256 = "gb43UufuN6iRu6MfTZP+a3GUUQ+hKa5e6G4IiWEc68A=";
    };
  };

in {
  options.stylix.targets.foot.enable =
    config.lib.stylix.mkEnableTarget "Foot" true;

  config = lib.mkIf cfg.enable {
    programs.foot.settings = {
      main = {
          include = "${config.xdg.configHome}/foot/stylix.ini";
          font =
            with config.stylix.fonts;
            "${monospace.name}:size=${toString sizes.terminal}";
          dpi-aware = "no";
      };
      colors.alpha = with config.stylix.opacity; terminal;
    };
    xdg.configFile."foot/stylix.ini".source = theme;
  };
}
