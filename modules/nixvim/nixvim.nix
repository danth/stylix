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

  options.lib.stylix.nixvim.config = lib.mkOption {
    type = with lib.types; attrsOf anything;
    readOnly = true;

    description = ''
      The stylix configuration, generated for nixvim.

      If nixvim is installed via nixos, darwin, or home-manager then this will be **automatically**
      assigned to `programs.nixvim`. If you're using a "standalone" build of nixvim, then that's
      not possible. Instead, you should pass this config to the `nixvimExtend` function.

      For example:

      ```nix
        { config, ... }: {
          environment.systemPackages = [
            (standalone-nixvim-derivation.nixvimExtend config.stylix.targets.nixvim.config)
          ];
        }
      ```

      See nixvim's docs on [extending a standalone configuration](https://nix-community.github.io/nixvim/modules/standalone.html#extending-an-existing-configuration).
    '';
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

  config = {
    programs = lib.mkIf (config.stylix.enable && cfg.enable) (
      lib.optionalAttrs (options.programs ? nixvim) {
        nixvim = config.lib.stylix.nixvim.config;
      }
    );
    lib.stylix.nixvim.config = lib.mkMerge [
      (lib.mkIf (cfg.plugin == "base16-nvim") {
        nixvim.colorschemes.base16 = {
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
        nixvim.plugins.mini = {
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
    ];
  };
}
