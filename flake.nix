{
  description = "Theming framework for NixOS, Home Manager, nix-darwin, and Nix-on-Droid";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    systems.url = "github:nix-systems/default";

    # keep-sorted start block=yes newline_separated=yes
    base16-fish = {
      url = "github:tomyun/base16-fish";
      flake = false;
    };

    base16-helix = {
      url = "github:tinted-theming/base16-helix";
      flake = false;
    };

    base16-vim = {
      # TODO: Unlock this input once [1] ("Seemingly bad parsing of whitespace
      # in abbriviated lists (affecting stylix's handling of base16-vim)") is
      # resolved, preventing us from fetching commit [2] ("fix(theme): Remove
      # illegal style attributes").
      #
      # [1]: https://github.com/SenchoPens/fromYaml/issues/1
      # [2]: https://github.com/tinted-theming/tinted-vim/commit/0508601eff146db2537eff23e93dd0c543914896
      url = "github:tinted-theming/base16-vim/577fe8125d74ff456cf942c733a85d769afe58b7";
      flake = false;
    };

    base16.url = "github:SenchoPens/base16.nix";

    firefox-gnome-theme = {
      url = "github:rafaelmardojai/firefox-gnome-theme";
      flake = false;
    };

    flake-compat.url = "github:edolstra/flake-compat";

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
      };
    };

    gnome-shell = {
      # TODO: Unlocking the input and pointing to official repository requires
      # updating the patch:
      # https://github.com/nix-community/stylix/pull/224#discussion_r1460339607.
      url = "github:GNOME/gnome-shell/48.1";
      flake = false;
    };

    # The 'home-manager' input is used to generate the documentation.
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };

    tinted-foot = {
      # Lock the tinted-foot input to prevent upstream breaking changes.
      #
      # Considering that Stylix eventually re-implements this input's
      # functionality [1], it might be easiest to lock this input to avoid
      # wasted maintenance effort.
      #
      # [1]: https://github.com/nix-community/stylix/issues/571
      url = "github:tinted-theming/tinted-foot/fd1b924b6c45c3e4465e8a849e67ea82933fcbe4";
      flake = false;
    };

    tinted-kitty = {
      url = "github:tinted-theming/tinted-kitty";
      flake = false;
    };

    tinted-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    tinted-tmux = {
      url = "github:tinted-theming/tinted-tmux";
      flake = false;
    };

    tinted-zed = {
      url = "github:tinted-theming/base16-zed";
      flake = false;
    };
    # keep-sorted end
  };

  outputs =
    { flake-parts, systems, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ ./flake ];

      systems = import systems;
    };
}
