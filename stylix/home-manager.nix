{ lib, config, ... }:

with lib;

let
  cfg = config.stylix;

in {
  options.stylix = {
    homeManagerUsers = mkOption {
      type = types.listOf types.str;
      description = "Users for which to enable Home Manager integration.";
      default = [];
    };

    homeModule = mkOption {
      internal = true;
      description = "Home Manager module to apply for all users.";
    };
  };

  config = {
    home-manager.users = genAttrs cfg.homeManagerUsers (user: cfg.homeModule);
  };
}
