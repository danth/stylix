{ lib, args }:

path: default:
if (args ? "osConfig" && args.osConfig.stylix.homeManagerIntegration.enable)
  then lib.attrByPath path default args.osConfig.stylix
  else default
