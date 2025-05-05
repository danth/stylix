{ lib, ... }:
{
  name = "Lazygit";
  homepage = "https://github.com/jesseduffield/lazygit";
  maintainers = with lib.maintainers; [
    mateusauler
    naho
  ];
}
