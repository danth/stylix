{
  name = "Spicetify";
  homepage = "https://github.com/Gerg-L/spicetify-nix";
  maintainers = [ ];
  description = ''
    This Stylix module leverages the modules provided by
    [Spicetify-Nix](https://github.com/Gerg-L/spicetify-nix).

    > [!IMPORTANT]
    > This module will have no effect unless the desired Spicetify module is properly
    > [installed](https://github.com/Gerg-L/spicetify-nix?tab=readme-ov-file#usage)
    > and
    > [imported](https://github.com/Gerg-L/spicetify-nix?tab=readme-ov-file#modules)
    > into your configuration.
    >
    > Ensure you are configuring this module on the same platform (NixOS, Home
    > Manager, Darwin) as where you installed Spicetify.
  '';
}
