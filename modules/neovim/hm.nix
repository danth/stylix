{ mkTarget, lib, ... }:
{
  imports = [
    ./neovim.nix
    (lib.modules.importApply ./neovide.nix mkTarget)
    ./nixvim.nix
    ./nvf.nix
    (lib.modules.importApply ./vim.nix mkTarget)
  ];
}
