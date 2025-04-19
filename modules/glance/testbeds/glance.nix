{ lib, pkgs, ... }:
let
  host = "127.0.0.1";

  package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
    extraPolicies.OverrideFirstRunPage = "http://${host}:${builtins.toString port}";
  };

  port = 1234;
in
{
  stylix.testbed.application = {
    enable = true;
    name = "firefox";
    inherit package;
  };

  home-manager.sharedModules = lib.singleton {
    programs.firefox = {
      enable = true;
      inherit package;
    };

    services.glance = {
      enable = true;

      settings = {
        pages = lib.singleton {
          columns = [
            {
              size = "small";

              widgets = lib.singleton {
                hide-location = false;
                hour-format = "24h";
                location = "Tokyo, Japan";
                show-area-name = true;
                type = "weather";
                units = "metric";
              };
            }

            {
              size = "full";

              widgets = [
                {
                  autofocus = true;
                  search-engine = "https://github.com/NixOS/nixpkgs/pulls?q=is%3Apr+is%3Aopen+{QUERY}";
                  type = "search";
                }

                {
                  type = "group";

                  widgets = lib.singleton {
                    collapse-after = 15;

                    feeds = lib.singleton {
                      title = "LessWrong";
                      url = "https://www.lesswrong.com/feed.xml?view=curated-rss";
                    };

                    style = "vertical-list";
                    type = "rss";
                  };
                }
              ];
            }

            {
              size = "small";
              widgets = [ { type = "calendar"; } ];
            }
          ];

          name = "Home";
        };

        server = { inherit host port; };
      };
    };
  };
}
