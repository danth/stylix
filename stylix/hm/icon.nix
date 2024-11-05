{ config, lib, pkgs, ... }:

let
	cfg = config.stylix.iconTheme;
	pcfg = config.stylix.polarity;
in {
	imports = [ ../icon.nix ];
	config = lib.mkIf (config.stylix.enable && pkgs.stdenv.hostPlatform.isLinux) {
		gtk = {
			enable = true;
			iconTheme = {
				package = cfg.iconTheme.package;
				name = builtins.head (lib.filter (x: !isNull x) [
				  ({
				    dark = cfg.iconTheme.dark;
				    light = cfg.iconTheme.light;
				  }."${pcfg.type}" or null)
				  cfg.iconTheme.dark
				  cfg.iconTheme.light
				]);
			};
		};
	};
}
