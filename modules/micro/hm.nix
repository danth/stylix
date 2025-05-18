{
  config,
  lib,
  ...
}:
{
  options.stylix.targets.micro.enable =
    config.lib.stylix.mkEnableTarget "micro" true;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.micro.enable) {
    # TODO: Provide a real colorscheme once [1] is resolved.
    #
    # [1]: https://github.com/nix-community/stylix/issues/249
    programs.micro.settings.colorscheme = "simple";
  };
}
