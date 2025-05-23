{
  lib,
  pkgs,
  config,
  options,
  ...
}:
let
  theme = pkgs.callPackage ./theme.nix {
    inherit (config.lib.stylix) colors;
    inherit (config.stylix) inputs;
  };
in
{
  overlay =
    lib.mkIf (options.stylix.targets ? gnome && config.stylix.targets.gnome.enable)
      (
        _: super: {
          gnome-shell = super.gnome-shell.overrideAttrs (oldAttrs: {
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
          });
        }
      );
}
