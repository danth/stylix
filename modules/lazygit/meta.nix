{ lib, ... }:
{
  name = "Lazygit";
  homepages = "https://github.com/jesseduffield/lazygit";
  maintainers = with lib.maintainers; [
    mateusauler
    naho
  ];
}
