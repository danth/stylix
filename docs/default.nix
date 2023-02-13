{ pkgs, pkgsLib, coricamuLib, inputs, ... }:

{
  baseUrl = "https://danth.github.io/stylix/";
  siteTitle = "Stylix";
  language = "en-gb";

  header.html = ''
    <h1>Stylix</h1>
    <nav>
      <a href="">Home</a>
      <a href="options.html">NixOS options</a>
      <a href="options-hm.html">Home Manager options</a>
      <a href="https://github.com/danth/stylix">GitHub repository</a>
    </nav>
  '';

  pages = [
    {
      path = "index.html";
      title = "Stylix";

      body.markdownFile = pkgs.runCommand "index.md" {} ''
        # Remove the title line
        tail -n+2 ${../README.md} >$out
      '';
    }

    {
      path = "options.html";
      title = "NixOS options";

      body.docbook =
        let
          configuration = pkgsLib.nixosSystem {
            inherit (pkgs) system;
            modules = [
              inputs.self.nixosModules.stylix
              ./settings.nix
              { _module.check = false; }
            ];
          };
        in
          coricamuLib.makeOptionsDocBook {
            inherit (configuration) options;
            customFilter = option: builtins.elemAt option.loc 0 == "stylix";
          };
    }

    {
      path = "options-hm.html";
      title = "Home Manager options";

      body.docbook =
        let
          configuration = inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              inputs.self.homeManagerModules.stylix
              ./settings.nix
              {
                home = {
                  homeDirectory = "/home/docs";
                  stateVersion = "22.11";
                  username = "docs";
                };
              }
            ];
            check = false;
          };
        in
          coricamuLib.makeOptionsDocBook {
            inherit (configuration) options;
            customFilter = option: builtins.elemAt option.loc 0 == "stylix";
          };
    }
  ];
}
