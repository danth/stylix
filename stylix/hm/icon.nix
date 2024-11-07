{ config, lib, pkgs, ... }:

let
	cfg = config.stylix.iconTheme;
	inherit (config.stylix) polarity;
in {
	imports = [ ../icon.nix ];
	config = lib.mkIf (config.stylix.enable && cfg.enable && pkgs.stdenv.hostPlatform.isLinux) {
		gtk = {
			iconTheme = {
				inherit (cfg) package;
				name = builtins.head (lib.filter (x: null != x) [
				  ({
					inherit (cfg) dark light;
				  }."${polarity}" or null)
				  cfg.dark
				  cfg.light
				]);
			};
		};
	};
}
