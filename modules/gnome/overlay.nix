{
  lib,
  config,
  pkgs,
  ...
}:
{
  overlay =
    lib.mkIf (config.stylix.enable && config.stylix.targets.gnome.enable or false)
      (
        _final: prev: {
          gnome-shell = prev.gnome-shell.overrideAttrs (
            oldAttrs:
            let
              theme = pkgs.callPackage ./theme.nix {
                inherit (config.lib.stylix) colors;
                inherit (config.stylix) inputs;
              };
            in
            {
              # Themes are usually applied via an extension, but extensions are
              # not available on the login screen. The only way to change the
              # theme there is by replacing the default.
              postFixup =
                (oldAttrs.postFixup or "")
                + ''
                  cp ${theme}/share/gnome-shell/gnome-shell-theme.gresource \
                    $out/share/gnome-shell/gnome-shell-theme.gresource
                '';
              patches = (oldAttrs.patches or [ ]) ++ [
                ./shell_remove_dark_mode.patch
              ];
            }
          );
        }
      );
}
