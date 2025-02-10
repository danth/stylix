{ colors, fonts }:
''
  /**
  * @name Stylix
  * @author Stylix
  * @version 0.0.0
  * @description Theme configured via NixOS or Home Manager.
  **/

  :root {
      --font-primary: ${fonts.sansSerif.name};
      --font-display: ${fonts.sansSerif.name};
      --font-code: ${fonts.monospace.name};
      --base00: #${colors.base00}; /* Black */
      --base01: #${colors.base01}; /* Bright Black */
      --base02: #${colors.base02}; /* Grey */
      --base03: #${colors.base03}; /* Brighter Grey */
      --base04: #${colors.base04}; /* Bright Grey */
      --base05: #${colors.base05}; /* White */
      --base06: #${colors.base06}; /* Brighter White */
      --base07: #${colors.base07}; /* Bright White */
      --base08: #${colors.base08}; /* Red */
      --base09: #${colors.base09}; /* Orange */
      --base0A: #${colors.base0A}; /* Yellow */
      --base0B: #${colors.base0B}; /* Green */
      --base0C: #${colors.base0C}; /* Cyan */
      --base0D: #${colors.base0D}; /* Blue */
      --base0E: #${colors.base0E}; /* Purple */
      --base0F: #${colors.base0F}; /* Magenta */

      --primary-630: var(--base00); /* Autocomplete background */
      --primary-660: var(--base00); /* Search input background */
  }

  .theme-light, .theme-dark {
      --search-popout-option-fade: none; /* Disable fade for search popout */
      --bg-overlay-2: var(--base00); /* These 2 are needed for proper threads coloring */
      --home-background: var(--base00);
      --bg-overlay-chat : var(--base00); /* Recolor forum channels */
      --background-primary: var(--base00);
      --background-secondary: var(--base01);
      --background-secondary-alt: var(--base01);
      --channeltextarea-background: var(--base01);
      --background-tertiary: var(--base00);
      --background-accent: var(--base0E);
      --background-floating: var(--base01);
      --background-modifier-hover: #{{base00-hex}}4c; /* 30% of base00 */
      --background-modifier-selected: var(--base00);
      --text-normal: var(--base05);
      --text-secondary: var(--base03);
      --text-muted: var(--base04);
      --text-link: var(--base0C);
      --interactive-normal: var(--base05);
      --interactive-hover: var(--base05);
      --interactive-active: var(--base07);
      --interactive-muted: var(--base03);
      --channels-default: var(--base04);
      --channel-icon: var(--base04);
      --header-primary: var(--base06);
      --header-secondary: var(--base04);
      --scrollbar-thin-track: transparent;
      --scrollbar-auto-track: transparent;
  }
''
