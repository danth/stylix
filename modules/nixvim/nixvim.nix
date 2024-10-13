{
  config,
  lib,
  options,
  ...
}: {
  options.stylix.targets.nixvim = {
    enable =
      config.lib.stylix.mkEnableTarget "nixvim" true;
    transparentBackground = {
      main = lib.mkEnableOption "background transparency for the main NeoVim window";
      signColumn = lib.mkEnableOption "background transparency for the NeoVim sign column";
    };
  };

  imports = [
    (
      lib.mkRenamedOptionModuleWith {
        from = [ "stylix" "targets" "nixvim" "transparent_bg" "main" ];
        sinceRelease = 2411;
        to = [ "stylix" "targets" "nixvim" "transparentBackground" "main" ];
      }
    )

    (
      lib.mkRenamedOptionModuleWith {
        from = [ "stylix" "targets" "nixvim" "transparent_bg" "sign_column" ];
        sinceRelease = 2411;

        to = [
          "stylix"
          "targets"
          "nixvim"
          "transparentBackground"
          "signColumn"
        ];
      }
    )
  ];

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.nixvim.enable && (config.programs ? nixvim)) (
    lib.optionalAttrs (builtins.hasAttr "nixvim" options.programs) {
      programs.nixvim = {
        colorschemes.base16 = {
          colorscheme = {
            inherit (config.lib.stylix.colors.withHashtag)
              base00 base01 base02 base03 base04 base05 base06 base07
              base08 base09 base0A base0B base0C base0D base0E base0F;
          };

          enable = true;
        };

        highlight = let
          cfg = config.stylix.targets.nixvim;
          transparent = {
            bg = "none";
            ctermbg = "none";
          };
        in {
          Normal = lib.mkIf cfg.transparentBackground.main transparent;
          NonText = lib.mkIf cfg.transparentBackground.main transparent;
          SignColumn = lib.mkIf cfg.transparentBackground.signColumn transparent;
        };
      };
    }
  );
}
