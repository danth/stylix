{ config, lib, pkgs, ... }:

with config.lib.stylix.colors;

let
  shadow = {
      "dark" = base00;
      "light" = base07;
    }.${scheme-variant};
  base = base00;
  alt = base05;
  scheme = pkgs.writeText "Simp1e-Stylix.txt" ''
    name:Simp1e Stylix

    shadow:${shadow}
    shadow_opacity:0.35

    cursor_border:${alt}

    default_cursor_bg:${base}
    hand_bg:${base}

    question_mark_bg:${blue}
    question_mark_fg:${base}

    plus_bg:${green}
    plus_fg:${base}

    link_bg:${magenta}
    link_fg:${base}

    move_bg:${yellow}
    move_fg:${base}

    context_menu_bg:${cyan}
    context_menu_fg:${base}

    forbidden_bg:${base}
    forbidden_fg:${red}

    magnifier_bg:${base}
    magnifier_fg:${alt}

    skull_bg:${base}
    skull_eye:${alt}

    spinner_bg:${base}
    spinner_fg1:${alt}
    spinner_fg2:${alt}
  '';
  pkg = pkgs.simp1e-cursors.overrideAttrs (old: {
    preBuild = ''
      rm src/color_schemes/*
      cp ${scheme} src/color_schemes/Simp1e-Stylix.txt
    '';
  });

in {
  options.stylix.targets.simp1e.enable = 
  	config.lib.stylix.mkEnableTarget "Simp1e Cursors" false;

  config = lib.mkIf (config.stylix.enable && config.stylix.targets.simp1e.enable) {
    stylix.cursor = {
      package = pkg;
      name = "Simp1e-Stylix";
    };
  };
}