{ config, lib, ... }:

{
  options.stylix.targets.adwaita-icon-theme.enable =
    config.lib.stylix.mkEnableTarget "Adwaita icon theme" true;

  config.nixpkgs.overlays = lib.mkIf config.stylix.targets.adwaita-icon-theme.enable [
    (self: super: {
      gnome = super.gnome.overrideScope (gnomeSelf: gnomeSuper: {
        adwaita-icon-theme = gnomeSuper.adwaita-icon-theme.overrideAttrs (oldAttrs: {
          # 428be2 can be removed when the following commit is released:
          # https://github.com/GNOME/adwaita-icon-theme/commit/4533eff4a4800b84ad0a0d2d253d60e2fe9215f7

          postPatch =
            (oldAttrs.postPatch or "") +
            (with config.lib.stylix.colors; ''
              find . -name '*.svg' | while read -r file; do
                substituteInPlace "$file" \
                  --replace '#428be2' '#${base01}' \
                  --replace '#438de6' '#${base01}' \
                  --replace '#a4caee' '#${base02}' \
                  --replace '#afd4ff' '#00000000' \
                  --replace '#62a0ea' '#00000000' \
                  --replace '#c0d5ea' '#00000000'
              done
            '');
        });
      });
    })
  ];
}
