{
  config,
  lib,
  options,
  ...
}:
let
  cfg = config.stylix.targets.nixvim;
in
{
  options.stylix.targets.nixvim = {
    enable = config.lib.stylix.mkEnableTarget "nixvim" true;
    plugin = lib.mkOption {
      type = lib.types.enum [
        "base16-nvim"
        "mini.base16"
      ];
      default = "mini.base16";
      description = "Plugin used for the colorscheme";
    };
    transparentBackground = {
      main = lib.mkEnableOption "background transparency for the main NeoVim window";
      signColumn = lib.mkEnableOption "background transparency for the NeoVim sign column";
    };
  };

  imports = [
    (lib.mkRenamedOptionModuleWith {
      from = [
        "stylix"
        "targets"
        "nixvim"
        "transparent_bg"
        "main"
      ];
      sinceRelease = 2411;
      to = [
        "stylix"
        "targets"
        "nixvim"
        "transparentBackground"
        "main"
      ];
    })

    (lib.mkRenamedOptionModuleWith {
      from = [
        "stylix"
        "targets"
        "nixvim"
        "transparent_bg"
        "sign_column"
      ];
      sinceRelease = 2411;

      to = [
        "stylix"
        "targets"
        "nixvim"
        "transparentBackground"
        "signColumn"
      ];
    })
  ];

  config =
    lib.mkIf (config.stylix.enable && cfg.enable && (config.programs ? nixvim))
      (
        lib.optionalAttrs (builtins.hasAttr "nixvim" options.programs) (
          lib.mkMerge [
            (lib.mkIf (cfg.plugin == "base16-nvim") {
              programs.nixvim.colorschemes.base16 = {
                enable = true;

                colorscheme = {
                  inherit (config.lib.stylix.colors.withHashtag)
                    base00
                    base01
                    base02
                    base03
                    base04
                    base05
                    base06
                    base07
                    base08
                    base09
                    base0A
                    base0B
                    base0C
                    base0D
                    base0E
                    base0F
                    ;
                };
              };
            })
            (lib.mkIf (cfg.plugin == "mini.base16") {
              programs.nixvim.plugins.mini = {
                enable = true;

                modules.base16.palette = {
                  inherit (config.lib.stylix.colors.withHashtag)
                    base00
                    base01
                    base02
                    base03
                    base04
                    base05
                    base06
                    base07
                    base08
                    base09
                    base0A
                    base0B
                    base0C
                    base0D
                    base0E
                    base0F
                    ;
                };
              };
            })
            {
              programs.nixvim = {
                highlight =
                  let
                    transparent = {
                      bg = "none";
                      ctermbg = "none";
                    };
                  in
                  {
                    Normal = lib.mkIf cfg.transparentBackground.main transparent;
                    NonText = lib.mkIf cfg.transparentBackground.main transparent;
                    SignColumn = lib.mkIf cfg.transparentBackground.signColumn transparent;
                  };
              };
            }
          ]
        )
      );
}
