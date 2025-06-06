{ mkTarget, pkgs, ... }:
mkTarget {
  name = "emacs";
  humanName = "Emacs";

  configElements = [
    (
      { opacity }:
      {
        programs.emacs.extraConfig = ''
          (add-to-list 'default-frame-alist '(alpha-background . ${
            toString (builtins.ceil (opacity.applications * 100))
          }))
        '';
      }
    )
    (
      { fonts }:
      {
        programs.emacs.extraConfig = ''
          (set-face-attribute 'default nil :font (font-spec :family "${fonts.monospace.name}" :size ${
            toString (fonts.sizes.terminal * 1.0)
          }))
        '';
      }
    )
    (
      { colors }:
      {
        programs.emacs = {
          extraConfig = ''
            (require 'base16-stylix-theme)
            (setq base16-theme-256-color-source 'colors)
            (load-theme 'base16-stylix t)
          '';

          extraPackages = epkgs: [
            (epkgs.trivialBuild (
              with colors.withHashtag;
              {
                pname = "base16-stylix-theme";
                version = "0.1.0";
                src = pkgs.writeText "base16-stylix-theme.el" ''
                  (require 'base16-theme)

                  (defvar base16-stylix-theme-colors
                    '(:base00 "${base00}"
                      :base01 "${base01}"
                      :base02 "${base02}"
                      :base03 "${base03}"
                      :base04 "${base04}"
                      :base05 "${base05}"
                      :base06 "${base06}"
                      :base07 "${base07}"
                      :base08 "${base08}"
                      :base09 "${base09}"
                      :base0A "${base0A}"
                      :base0B "${base0B}"
                      :base0C "${base0C}"
                      :base0D "${base0D}"
                      :base0E "${base0E}"
                      :base0F "${base0F}")
                    "All colors for Base16 stylix are defined here.")

                  ;; Define the theme
                  (deftheme base16-stylix)

                  ;; Add all the faces to the theme
                  (base16-theme-define 'base16-stylix base16-stylix-theme-colors)

                  ;; Mark the theme as provided
                  (provide-theme 'base16-stylix)

                  ;; Add path to theme to theme-path
                  (add-to-list 'custom-theme-load-path
                      (file-name-directory
                          (file-truename load-file-name)))

                  (provide 'base16-stylix-theme)
                '';
                packageRequires = [ epkgs.base16-theme ];
              }
            ))
          ];
        };
      }
    )
  ];
}
