{
  description = "Theming framework for NixOS, Home Manager, nix-darwin, and Nix-on-Droid";

  inputs = {
    base16-fish = {
      flake = false;
      url = "github:tomyun/base16-fish";
    };

    base16-helix = {
      flake = false;
      url = "github:tinted-theming/base16-helix";
    };

    base16-vim = {
      flake = false;

      # TODO: Unlock this input once [1] ("Seemingly bad parsing of whitespace
      # in abbriviated lists (affecting stylix's handling of base16-vim)") is
      # resolved, preventing us from fetching commit [2] ("fix(theme): Remove
      # illegal style attributes").
      #
      # [1]: https://github.com/SenchoPens/fromYaml/issues/1
      # [2]: https://github.com/tinted-theming/tinted-vim/commit/0508601eff146db2537eff23e93dd0c543914896
      url = "github:tinted-theming/base16-vim/577fe8125d74ff456cf942c733a85d769afe58b7";
    };

    base16.url = "github:SenchoPens/base16.nix";

    gnome-shell = {
      flake = false;

      # TODO: Unlocking the input and pointing to official repository requires
      # updating the patch:
      # https://github.com/danth/stylix/pull/224#discussion_r1460339607.
      url = "github:GNOME/gnome-shell/47.2";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Interface flake systems.
    systems.url = "github:nix-systems/default";

    tinted-foot = {
      flake = false;

      # Lock the tinted-foot input to prevent upstream breaking changes.
      #
      # Considering that Stylix eventually re-implements this input's
      # functionality [1], it might be easiest to lock this input to avoid
      # wasted maintenance effort.
      #
      # [1]: https://github.com/danth/stylix/issues/571
      url = "github:tinted-theming/tinted-foot/fd1b924b6c45c3e4465e8a849e67ea82933fcbe4";
    };

    tinted-zed = {
      flake = false;
      url = "github:tinted-theming/base16-zed";
    };

    tinted-tmux = {
      flake = false;
      url = "github:tinted-theming/tinted-tmux";
    };

    tinted-kitty = {
      flake = false;

      # Lock the tinted-kitty input to prevent upstream breaking changes.
      #
      # Considering that Stylix eventually re-implements this input's
      # functionality [1], it might be easiest to lock this input to avoid
      # wasted maintenance effort.
      #
      # [1]: https://github.com/danth/stylix/issues/534
      url = "github:tinted-theming/tinted-kitty/eb39e141db14baef052893285df9f266df041ff8";
    };

    tinted-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    firefox-gnome-theme = {
      flake = false;
      url = "github:rafaelmardojai/firefox-gnome-theme";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ ./flake ];

      systems = import inputs.systems;
    };
}
