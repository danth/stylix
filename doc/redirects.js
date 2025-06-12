(function () {
  // A table of paths to redirect from, mapped to the new location.
  // NOTE: This script doesn't run on 404.html, so the old page must still exist.
  const redirects = {
    "/configuration.html#standalone-nixvim":
      "./options/modules/neovim.html#standalone-mode",
  };

  const fullPath = window.location.pathname + window.location.hash;

  // Remove baseHref
  const path =
    fullPath.indexOf("/stylix/") == 0 ? fullPath.substring(7) : fullPath;

  const target = redirects[path];

  if (target) {
    console.log("Redirecting to " + target);
    window.location.replace(target);
  }
})();
