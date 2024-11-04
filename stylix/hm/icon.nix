{ config, lib, pkgs, ... }:

let
	cfg = config.stylix.icon;
	pcfg = config.stylix.polarity;
in {
	imports = [ ../icon.nix ];
	config = lib.mkIf (config.stylix.enable && pkgs.stdenv.hostPlatform.isLinux) {
		gtk = {
			enable = true;
			iconTheme = {
				package = cfg.iconTheme.package;
				name = if pcfg.type == "light" then cfg.iconTheme.light
				else if pcfg.type == "dark" then cfg.iconTheme.dark
				else cfg.iconTheme.dark;
			};
		};
	};
}
