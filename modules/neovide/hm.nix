{ mkTarget, ... }:
mkTarget {
  name = "neovide";
  humanName = "Neovide";
  configElements = [
    (
      { fonts }:
      {
        programs.neovide.settings.font = {
          normal = [ fonts.monospace.name ];
          size = fonts.sizes.terminal;
        };
      }
    )
    (
      { opacity }:
      {
        programs.neovim.extraLuaConfig = ''
          if vim.g.neovide then
            vim.g.neovide_normal_opacity = ${toString opacity.terminal}
          end
        '';
      }
    )
  ];
}
