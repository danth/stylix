# Firefox and its derivatives

This module supports [Firefox](https://www.mozilla.org/firefox/), in addition
to [Floorp](https://floorp.app/) and [LibreWolf](https://librewolf.net/),
which are Firefox derivatives.

The same implementation is shared between all of these browsers, but they don't
share option values.

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
