{
  config,
  lib,
  options,
  ...
}: {
  options.stylix.targets.nixvim = {
    enable =
      config.lib.stylix.mkEnableTarget "nixvim" (config.programs ? nixvim);
    transparent_bg = {
      main = lib.mkEnableOption "background transparency for the main NeoVim window";
      sign_column = lib.mkEnableOption "background transparency for the NeoVim sign column";
    };
    config = lib.mkOption {
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
  };

  config = {
    programs =
      lib.optionalAttrs
      (options.programs ? nixvim)
      (lib.mkIf config.stylix.targets.nixvim.enable {
        nixvim = config.stylix.targets.nixvim.config;
      });

    stylix.targets.nixvim.config = {
      colorschemes.base16 = {
        enable = true;

        colorscheme = let
          colors = config.lib.stylix.colors.withHashtag;
        in {
          base00 = colors.base00;
          base01 = colors.base01;
          base02 = colors.base02;
          base03 = colors.base03;
          base04 = colors.base04;
          base05 = colors.base05;
          base06 = colors.base06;
          base07 = colors.base07;
          base08 = colors.base08;
          base09 = colors.base09;
          base0A = colors.base0A;
          base0B = colors.base0B;
          base0C = colors.base0C;
          base0D = colors.base0D;
          base0E = colors.base0E;
          base0F = colors.base0F;
        };
      };

      highlight = let
        cfg = config.stylix.targets.nixvim;
        transparent = {
          bg = "none";
          ctermbg = "none";
        };
      in {
        Normal = lib.mkIf cfg.transparent_bg.main transparent;
        NonText = lib.mkIf cfg.transparent_bg.main transparent;
        SignColumn = lib.mkIf cfg.transparent_bg.sign_column transparent;
      };
    };
  };
}
