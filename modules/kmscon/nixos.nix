{ config, lib, ... }:
{
  options.stylix.targets.kmscon.enable =
    config.lib.stylix.mkEnableTarget "the kmscon virtual console" true;

  config.services.kmscon =
    lib.mkIf (config.stylix.enable && config.stylix.targets.kmscon.enable)
      {
        fonts = [ config.stylix.fonts.monospace ];
        extraConfig =
          let
            formatBase =
              name:
              let
                getComponent = comp: config.lib.stylix.colors."${name}-rgb-${comp}";
              in
              "${getComponent "r"},${getComponent "g"},${getComponent "b"}";
          in
          ''
            font-size=${builtins.toString config.stylix.fonts.sizes.terminal}

            palette=custom

            palette-black=${formatBase "base00"}
            palette-red=${formatBase "base08"}
            palette-green=${formatBase "base0B"}
            palette-yellow=${formatBase "base0A"}
            palette-blue=${formatBase "base0D"}
            palette-magenta=${formatBase "base0E"}
            palette-cyan=${formatBase "base0C"}
            palette-light-grey=${formatBase "base05"}
            palette-dark-grey=${formatBase "base03"}
            palette-light-red=${formatBase "base08"}
            palette-light-green=${formatBase "base0B"}
            palette-light-yellow=${formatBase "base0A"}
            palette-light-blue=${formatBase "base0D"}
            palette-light-magenta=${formatBase "base0E"}
            palette-light-cyan=${formatBase "base0C"}
            palette-white=${formatBase "base07"}

            palette-background=${formatBase "base00"}
            palette-foreground=${formatBase "base05"}
          '';
      };
}
