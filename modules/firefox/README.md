# Firefox and derivatives

This module supports [Firefox](https://www.mozilla.org/firefox/), and its
derivatives [Floorp](https://floorp.app/) and
[LibreWolf](https://librewolf.net/), using the same underlying implementation.
Each browser has its own set of options.

> [!IMPORTANT]
>
> For any theming to be applied, you need to tell this module which
> [profiles](https://support.mozilla.org/en-US/kb/profiles-where-firefox-stores-user-data)
> you're using:
>
> ```nix
> {
>   programs.firefox = {
>     enable = true;
>
>     profiles = {
>       my-profile = {
>         # bookmarks, extensions, search engines...
>       };
>       my-friends-profile = {
>         # bookmarks, extensions, search engines...
>       };
>     };
>   };
>
>   stylix.targets.firefox.profileNames = [ "my-profile" "my-friends-profile" ];
> }
> ```
>
> This is necessary due to a limitation of the module system: we can either
> detect the list of profiles, or change their configuration, but we can't do
> both without infinite recursion.
