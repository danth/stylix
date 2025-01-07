# Home Manager options

The following options can only be set in a Home Manager configuration.

If you combined Home Manager with your NixOS configuration, write these
options within a Home Manager section, either for all users:

```nix
home-manager.sharedModules = [{
  stylix.targets.xyz.enable = false;
}];
```

Or for a specific user:

```nix
home-manager.users.«name» = {
  stylix.targets.xyz.enable = false;
};
```

[Read more about per-user themes.](../configuration.md#multi-user-configurations)

<!-- Auto-generated documentation will be added below. -->
