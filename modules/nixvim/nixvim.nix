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
    config = lib.mkOption {
      type = with lib.types; attrsOf anything;
      readOnly = true;

      description = ''
        The stylix configuration, generated for nixvim.
        If nixvim is installed via nixos, darwin, or home-manager then this will be **automatically**
        assigned to `programs.nixvim`. If you're using a "standalone" build of nixvim, then that's
        not possible. Instead, you should pass this config to the `extend` function.
        For example:
        ```nix
          { config, ... }: {
            environment.systemPackages = [
              (inputs.<your-nixvim-input>.<your-system>.default.extend config.stylix.targets.nixvim.config)
            ];
          }
        ```
        See nixvim's docs on [extending a standalone configuration](https://nix-community.github.io/nixvim/platforms/standalone.html?highlight=extend#extending-an-existing-configuration).
      '';
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

  config = lib.mkIf (config.stylix.enable && cfg.enable) {
    stylix.targets.nixvim.config = lib.mkMerge [
      (lib.mkIf (cfg.plugin == "base16-nvim") {
        colorschemes.base16 = {
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
        plugins.mini = {
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
      }
    ];

    programs = lib.optionalAttrs (builtins.hasAttr "nixvim" options.programs) (
      lib.mkIf config.stylix.targets.nixvim.enable {
        nixvim = config.stylix.targets.nixvim.config;
      }
    );
  };
}
