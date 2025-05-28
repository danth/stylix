# imported from `modules/nixvim/nixvim.nix`
stylix: {
  extraConfigLua =
    # lua
    ''
      if vim.g.neovide then
        vim.g.neovide_normal_opacity = ${toString stylix.opacity.terminal}
      end
    '';
}
