{
  # Old attribute names for backwards compatibility
  services.xserver = {
    enable = true;
    desktopManager = {
      plasma5.enable = true;
      plasma6.enable = true;
    };
    displayManager.sddm.enable = true;
  };

  # New attribute names since 3rd of 2024-03-12 (https://github.com/NixOS/nixpkgs/commit/b07cdeb1b34503576ec4aba981740466d19cb8e5)
  # There is no Plasma 5 equivalent for this
  services.desktopManager.plasma6.enable = true;
}
