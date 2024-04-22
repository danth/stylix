{ lib, args }:

path: default:
if ( args ? "osConfig"
  && args.osConfig ? "stylix"
  && args.osConfig.stylix.homeManagerIntegration.followSystem)
  then lib.attrByPath path default args.osConfig.stylix
  else default
