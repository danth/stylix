{ config, lib, pkgs, ... }:

let
	cfg = config.stylix.iconTheme;
	polarity = config.stylix.polarity;
in {
	imports = [ ../icon.nix ];
	config = lib.mkIf (config.stylix.enable && cfg.enable && pkgs.stdenv.hostPlatform.isLinux) {
		gtk = {
			enable = true;
			iconTheme = {
				package = cfg.package;
				name = builtins.head (lib.filter (x: !isNull x) [
				  ({
				    dark = cfg.dark;
				    light = cfg.light;
				  }."${polarity}" or null)
				  cfg.dark
				  cfg.light
				  ""
				]);
			};
		};
	};
}
