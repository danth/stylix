{ mkTarget, ... }:
mkTarget {
  name = "micro";
  humanName = "micro";

  configElements = {
    # TODO: Provide a real colorscheme once [1] is resolved.
    #
    # [1]: https://github.com/nix-community/stylix/issues/249
    programs.micro.settings.colorscheme = "simple";
  };
}
