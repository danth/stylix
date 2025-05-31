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
        lib.throwIf (addEnableOption && humanName == null)
          "adding enable option requires setting humanName"
          {
            options.stylix.targets.${name} = lib.mkMerge [
              (lib.mkIf addEnableOption {
                enable = config.lib.stylix.mkEnableTarget humanName autoEnable;
              })
              extraOptions
            ];

            config.nixpkgs.overlays = lib.mkIf (
              config.stylix.enable
              && config.stylix.overlays.enable
              && config.stylix.targets.${name}.enable or false
            ) [ overlay ];
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
