{ lib, ... }:
{
  name = "Neovim, Neovide, NixVim, nvf, and Vim";
  homepage = {
    Neovim = "https://www.neovim.io/";
    Neovide = "https://neovide.dev/";
    NixVim = "https://github.com/nix-community/nixvim";
    nvf = "https://github.com/NotAShelf/nvf";
    Vim = "http://www.vim.org/";
  };
  maintainers = with lib.maintainers; [
    butzist
    mightyiam
    naho
  ];
  description = ''
    ### Neovim

    This target themes [Neovim] using the standard Home Manager options.

    ### NixVim

    This target themes [Neovim] using the options provided by [NixVim].

    > [!IMPORTANT]
    > This target will have no effect unless the desired Nixvim module is properly
    > [installed](https://github.com/nix-community/nixvim?tab=readme-ov-file#installation)
    > and
    > [imported](https://github.com/nix-community/nixvim?tab=readme-ov-file#usage)
    > into your configuration.
    >
    > Ensure you are configuring this target on the same platform (NixOS, Home
    > Manager, Darwin) as where you installed Nixvim.

    #### Standalone Mode

    When using a NixOS or home-manager installation of [NixVim], you can use Stylix
    as normal. However, when using Nixvim's ["standalone" configuration mode][NixVim Standalone],
    you will need to pass Stylix's generated config to Nixvim yourself.

    The generated config can be accessed as `config.lib.stylix.nixvim.config`. You
    can use this as a module in your standalone Nixvim Configuration or an
    extension of it.

    For example:

    ```nix
    {
      inputs,
      config,
      pkgs,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
      nixvim-package = inputs.nixvim-config.packages.''${system}.default;
      extended-nixvim = nixvim-package.extend config.lib.stylix.nixvim.config;
    in
    {
      environment.systemPackages = [ extended-nixvim ];
    }
    ```

    ### nvf

    This target themes [Neovim] using the options provided by [nvf].

    > [!IMPORTANT]
    > This target will have no effect unless the desired nvf module is properly
    > [installed](https://notashelf.github.io/nvf/index.xhtml#ch-installation)
    > and
    > [imported](https://notashelf.github.io/nvf/index.xhtml#ch-module-installation)
    > into your configuration.
    >
    > Ensure you are configuring this target on the same platform (NixOS, Home
    > Manager, Darwin) as where you installed nvf.

    ### Vim

    This target themes [Vim] using the standard Home Manager options.

    [Neovim]: https://neovim.io
    [Nixvim]: https://github.com/nix-community/nixvim#readme
    [NixVim Standalone]: https://nix-community.github.io/nixvim/user-guide/install.html#standalone-usage
    [nvf]: https://github.com/NotAShelf/nvf#readme
    [Vim]: https://www.vim.org
  '';
}
