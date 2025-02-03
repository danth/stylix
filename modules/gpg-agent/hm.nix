{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.stylix.targets.gpg-agent.enable =
    config.lib.stylix.mkEnableTarget "gpg-agent" true;

  config = lib.mkIf (
    config.stylix.enable && config.stylix.targets.gpg-agent.enable
  ) { services.gpg-agent.pinentryPackage = pkgs.pinentry-qt; };
}
