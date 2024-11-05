{ config, lib, pkgs, ... }:

let
	cfg = config.stylix.iconTheme;
	polarity = config.stylix.polarity;
in {
	imports = [ ../icon.nix ];
	config = lib.mkIf (config.stylix.enable && pkgs.stdenv.hostPlatform.isLinux) {
		gtk = {
			inherit (cfg) enable;
			iconTheme = {
				inherit (cfg) package;
				name = builtins.head (lib.filter (x: !isNull x) [
				  ({
					inherit (cfg) dark light;
				  }."${polarity}" or null)
				  cfg.dark
				  cfg.light
				  ""
				]);
			};
		};
	};
}
