{
  pkgs,
  lib,
  inputs,
  ...
}:

let
  nixosConfiguration = lib.nixosSystem {
    inherit (pkgs) system;
    modules = [
      inputs.home-manager.nixosModules.home-manager
      inputs.self.nixosModules.stylix
      ./settings.nix
    ];
  };

  homeManagerConfiguration = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      inputs.self.homeManagerModules.stylix
      ./settings.nix
      {
        home = {
          homeDirectory = "/home/book";
          stateVersion = "22.11";
          username = "book";
        };
      }
    ];
  };

  # TODO: Include Nix Darwin options

  # transformDeclaration =
  #   declaration:
  #   let
  #     declarationString = toString declaration;
  #     declarationWithoutprefix = lib.removePrefix "${declarationPrefix}/" declarationString;
  #   in
  #   lib.throwIfNot (lib.hasPrefix declarationPrefix declarationString)
  #     "declaration not in ${declarationPrefix}: ${declarationString}"
  #     {
  #       name = "<${declarationWithoutprefix}>";
  #       url = "https://github.com/danth/stylix/blob/${commit}/${declarationWithoutprefix}";
  #     };

  insert =
    {
      index,
      page,
      emptyPage,
      platform,
      option,
    }:
    index
    // {
      ${page} =
        let
          oldPage = index.${page} or emptyPage;
        in
        oldPage
        // {
          optionsByPlatform = oldPage.optionsByPlatform // {
            ${platform} = oldPage.optionsByPlatform.${platform} ++ [ option ];
          };
        };
    };

  insertDeclaration =
    {
      index,
      declaration,
      platform,
      option,
    }:
    if lib.hasPrefix "${inputs.self}/" declaration then
      let
        path = lib.removePrefix "${inputs.self}/" declaration;
        pathComponents = lib.splitString "/" path;
      in
      if builtins.elemAt pathComponents 0 == "modules" then
        let
          module = builtins.unsafeDiscardStringContext (builtins.elemAt pathComponents 1);
        in
        insert {
          inherit index platform option;
          page = "src/options/modules/${module}.md";
          emptyPage = {
            referenceSection = "Modules";
            readme = "${inputs.self}/modules/${module}/README.md";
            defaultReadme = ''
              # ${module}
              > [!NOTE]
              > This module doesn't include any additional documentation.
              > You can browse the options it provides below.
            '';
            optionsByPlatform = {
              # Module pages contain a section for all platforms, initialised
              # to an empty list so that *None provided.* is shown rather than
              # omitting the section.
              home_manager = [ ];
              nixos = [ ];
            };
          };
        }
      else
        insert {
          inherit index platform option;
          page = "src/options/platforms/${platform}.md";
          emptyPage = {
            referenceSection = "Platforms";
            readme = "${inputs.self}/docs/src/options/platforms/${platform}.md";
            defaultReadme = ''
              # ${platform}
              > Documentation is not available for this platform. Its main
              > options are listed below, and you may find more specific
              > options in the documentation for each module.
            '';
            optionsByPlatform = {
              # Platform pages only contain a section for that platform
              ${platform} = [ ];
            };
          };
        }
    else
      index;

  insertOption =
    {
      index,
      platform,
      option,
    }:
    builtins.foldl' (
      foldIndex: declaration:
      insertDeclaration {
        index = foldIndex;
        inherit declaration platform option;
      }
    ) index option.declarations;

  insertPlatform =
    {
      index,
      platform,
      configuration,
    }:
    builtins.foldl' (
      foldIndex: option:
      insertOption {
        index = foldIndex;
        inherit platform option;
      }
    ) index (lib.optionAttrSetToDocList configuration.options);

  index =
    builtins.foldl'
      (
        foldIndex:
        { platform, configuration }:
        insertPlatform {
          index = foldIndex;
          inherit platform configuration;
        }
      )
      { }
      [
        {
          platform = "home_manager";
          configuration = homeManagerConfiguration;
        }
        {
          platform = "nixos";
          configuration = nixosConfiguration;
        }
      ];

  renderOption =
    option:
    lib.optionalString (option.visible && !option.internal) ''
      ### ${option.name}
      ${option.description}
    '';

  renderPlatform =
    name: options:
    let
      sortedOptions = builtins.sort (a: b: a.name < b.name) options;
      renderedOptions =
        if sortedOptions == [ ] then
          "*None provided.*"
        else
          lib.concatMapStrings renderOption sortedOptions;
    in
    ''
      ## ${name} options
      ${renderedOptions}
    '';

  renderPage =
    _path: page:
    let
      readme =
        # This doesn't count as IFD because ${inputs.self} is a flake input
        if builtins.pathExists page.readme then
          builtins.readFile page.readme
        else
          page.defaultReadme;
      options = lib.concatStrings (
        lib.mapAttrsToList renderPlatform page.optionsByPlatform
      );
    in
    lib.concatLines [
      readme
      options
    ];

  renderedPages = lib.mapAttrs renderPage index;

  insertPageSummary =
    summary: path: page:
    let
      text = renderedPages.${path};
      lines = lib.splitString "\n" text;
      firstLine = builtins.elemAt lines 0;
      title = lib.removePrefix "# " firstLine;
      relativePath = lib.removePrefix "src/" path;
      entry =
        if title == firstLine then
          builtins.throw "${path} must begin with a title"
        else
          "  - [${title}](${relativePath})";
    in
    summary
    // {
      ${page.referenceSection} = (summary.${page.referenceSection} or [ ]) ++ [
        entry
      ];
    };

  summary = lib.foldlAttrs insertPageSummary { } index;

  renderSummarySection =
    referenceSection: entries:
    let
      parentEntry = "- [${referenceSection}]()";
    in
    [ parentEntry ] ++ entries;

  renderedSummary = lib.concatLines (
    lib.flatten (lib.mapAttrsToList renderSummarySection summary)
  );

  writePage = path: text: ''
    mkdir --parents ${lib.escapeShellArg (builtins.dirOf path)}
    echo ${lib.escapeShellArg text} >${lib.escapeShellArg path}
  '';

  writePages = lib.concatStrings (lib.mapAttrsToList writePage renderedPages);

in
pkgs.stdenvNoCC.mkDerivation {
  name = "stylix-book";
  src = ./.;
  buildInputs = with pkgs; [
    mdbook
    mdbook-alerts
  ];

  patchPhase = ''
    ${writePages}
    echo -n ${lib.escapeShellArg renderedSummary} >>src/SUMMARY.md
    cp ${../README.md} src/README.md
    cp ${../gnome.png} src/gnome.png
    cp ${../kde.png} src/kde.png
  '';

  buildPhase = "mdbook build --dest-dir $out";
}
