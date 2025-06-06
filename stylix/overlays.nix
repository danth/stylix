{ lib, config, ... }:
{
  options.stylix.overlays.enable = config.lib.stylix.mkEnableTarget "packages via overlays" true;

  imports = map (
    file:
    {
      lib,
      pkgs,
      config,
      options,
      ...
    }:
    let
      mkOverlay =
        {
          name,
          humanName ? null,
          addEnableOption ? false,
          autoEnable ? true,
          extraOptions ? { },
          overlay,
        }:
        let
          cfg = config.stylix.targets.${name};
          inherit (import ./mk-target/lib.nix { inherit cfg lib config; })
            mkConditionalConfig
            ;
        in
        {
          imports = [
            (lib.optionalAttrs addEnableOption {
              options.stylix.targets.${name}.enable =
                config.lib.stylix.mkEnableTarget humanName autoEnable;
            })
          ];

          options.stylix.targets.${name} = extraOptions;

          config.nixpkgs.overlays = lib.mkIf (
            config.stylix.enable && config.stylix.overlays.enable && cfg.enable or false
          ) [ (mkConditionalConfig overlay) ];
        };
      file' = import file;
      module =
        if builtins.isFunction file' then
          file' {
            inherit
              lib
              pkgs
              config
              options
              ;
          }
        else
          file';
    in
    {
      key = file;
      _file = file;
      imports = [ (mkOverlay module) ];
    }
  ) (import ./autoload.nix { inherit lib; } "overlay");
}
