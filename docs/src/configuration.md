# Configuration

## Getting started

The options you will be most interested in are as follows:

- Choosing a wallpaper: [`stylix.wallpaper`](./options/nixos.html#stylixwallpaper)
- Choosing a color scheme: [`stylix.colors`](./options/nixos.html#stylixcolors)
- Changing fonts: [`stylix.fonts.*`](./options/nixos.html#stylixfontsemoji)
- Making apps transparent: [`stylix.opacity.*`](./options/nixos.html#stylixopacityapplications)

Only the wallpaper is required as the others default to something reasonable.

Rebuild the configuration to apply any changed settings. Most software needs to
be restarted for a new theme to take effect, so you may wish to reboot too.

## Turning targets on and off

A target is anything which can have colors, fonts or a wallpaper applied to
it.

Each target has an option like `stylix.targets.«target».enable` to choose
whether it gets themed. By default, targets are enabled automatically when
they are installed - you can opt out of this via
[`stylix.autoEnable`](./options/nixos.html#stylixautoenable).

The available targets differ between Home Manager and NixOS. If a target
is available in both, it is always fine to enable both.

The reference pages have a list of targets for [NixOS](options/nixos.md) and
[Home Manager](options/hm.md) respectively.

## Multi-user configurations

For those apps which are configured through Home Manager, Stylix can set a
different theme for each user.

If Home Manager and NixOS are configured together, then the system theme will
be copied into the Home Manager configuration for each user. You can adjust
the settings in Home Manager for a particular user, and this will override the
system theme.

If you only want Stylix to affect certain accounts, you can set
`stylix.homeManagerIntegration.autoImport = false`, then manually import Stylix
into Home Manager for the accounts which should be themed.
