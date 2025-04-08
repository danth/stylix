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

      /* Copyright (c) 2025 Cole Schaefer

      Permission is hereby granted, free of charge, to any person obtaining a copy
      of this software and associated documentation files (the "Software"), to deal
      in the Software without restriction, including without limitation the rights
      to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      copies of the Software, and to permit persons to whom the Software is
      furnished to do so, subject to the following conditions:

      The above copyright notice and this permission notice shall be included in all
      copies or substantial portions of the Software.

      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      SOFTWARE. */

      .theme-light,
  .theme-dark,
  .theme-darker,
  .theme-midnight,
  .visual-refresh {
      --activity-card-background: var(--base01) !important;
      --background-accent: var(--base03) !important;
      --background-floating: var(--base02) !important;
      --background-mentioned-hover: var(--base02) !important;
      --background-mentioned: var(--base01) !important;
      --background-message-highlight: var(--base01) !important;
      --background-message-hover: var(--base00) !important;
      --background-modifier-accent: var(--base02) !important;
      --background-modifier-active: var(--base02) !important;
      --background-modifier-hover: var(--base00) !important;
      --background-modifier-selected: var(--base01) !important;
      --background-primary: var(--base00) !important;
      --background-secondary-alt: var(--base01) !important;
      --background-secondary: var(--base01) !important;
      --background-surface-highest: var(--base02) !important;
      --background-surface-higher: var(--base02) !important;
      --background-surface-high: var(--base02) !important;
      --background-tertiary: var(--base00) !important;
      --background-base-low: var(--base01) !important;
      --background-base-lower: var(--base00) !important;
      --background-base-lowest: var(--base00) !important;
      --background-base-tertiary: var(--base00) !important;
      --background-code: var(--base02) !important;
      --background-mod-subtle: var(--base02) !important;
      --bg-base-secondary: var(--base01) !important;
      --bg-base-tertiary: var(--base00) !important;
      --bg-brand: var(--base03) !important;
      --bg-mod-faint: var(--base01) !important;
      --bg-overlay-2: var(--base01) !important;
      --bg-overlay-3: var(--base01) !important;
      --bg-overlay-color-inverse: var(--base03) !important;
      --bg-surface-raised: var(--base02) !important;
      --bg-surface-overlay: var(--base00) !important;
      --black: var(--base00) !important;
      --blurple-50: var(--base0B) !important;
      --border-faint: var(--base02) !important;
      --brand-05a: var(--base01) !important;
      --brand-10a: var(--base01) !important;
      --brand-15a: var(--base01) !important;
      --brand-260: var(--base0D) !important;
      --brand-360: var(--base0D) !important;
      --brand-500: var(--base0F) !important;
      --brand-560: var(--base01) !important;
      --button-danger-background: var(--base08) !important;
      --button-filled-brand-background: var(--base0D) !important;
      --button-filled-brand-background-hover: var(--base03) !important;
      --button-filled-brand-text: var(--base00) !important;
      --button-filled-brand-text-hover: var(--base05) !important;
      --button-outline-positive-border: var(--base0B) !important;
      --button-outline-danger-background-hover: var(--base08) !important;
      --button-outline-danger-border-hover: var(--base08) !important;
      --button-positive-background: var(--base0B) !important;
      --button-positive-background-hover: var(--base03) !important;
      --button-secondary-background: var(--base02) !important;
      --button-secondary-background-hover: var(--base03) !important;
      --card-primary-bg: var(--base02) !important;
      --channel-icon: var(--base04) !important;
      --channels-default: var(--base04) !important;
      --channel-text-area-placeholder: var(--base03) !important;
      --channeltextarea-background: var(--base01) !important;
      --chat-background-default: var(--base02) !important;
      --checkbox-background-checked: var(--base0D) !important;
      --checkbox-border-checked: var(--base0D) !important;
      --checkbox-background-default: var(--base02) !important;
      --checkbox-border-default: var(--base03) !important;
      --control-brand-foreground-new: var(--base0D) !important;
      --control-brand-foreground: var(--base04) !important;
      --custom-notice-text: var(--base01) !important;
      --font-display: var(--font, "gg sans") !important;
      --font-headline: var(--font, "gg sans") !important;
      --font-primary: var(--font, "gg sans") !important;
      --green-330: var(--base0B) !important;
      --green-360: var(--base0B) !important;
      --header-primary: var(--base04) !important;
      --header-secondary: var(--base04) !important;
      --home-background: var(--base00) !important;
      --info-warning-foreground: var(--base0A) !important;
      --input-background: var(--base02) !important;
      --interactive-active: var(--base05) !important;
      --interactive-hover: var(--base05) !important;
      --interactive-muted: var(--base03) !important;
      --interactive-normal: var(--base05) !important;
      --mention-background: var(--base03) !important;
      --mention-foreground: var(--base05) !important;
      --menu-item-danger-active-bg: var(--base08) !important;
      --menu-item-danger-hover-bg: var(--base08) !important;
      --menu-item-default-hover-bg: var(--base03) !important;
      --message-reacted-background: var(--base02) !important;
      --message-reacted-text: var(--base05) !important;
      --modal-background: var(--base01) !important;
      --modal-footer-background: var(--base00) !important;
      --notice-background-positive: var(--base0B) !important;
      --notice-text-positive: var(--base01) !important;
      --plum-23: var(--base02) !important;
      --primary-130: var(--base05) !important;
      --primary-300: var(--base05) !important;
      --primary-500: var(--base02) !important;
      --primary-600: var(--base00) !important;
      --primary-630: var(--base01) !important;
      --primary-660: var(--base00) !important;
      --primary-800: var(--base00) !important;
      --red-400: var(--base08) !important;
      --red-460: var(--base08) !important;
      --red-500: var(--base08) !important;
      --red-630: var(--base08) !important;
      --red: var(--base08) !important;
      --scrollbar-auto-thumb: var(--base00) !important;
      --scrollbar-auto-track: transparent;
      --scrollbar-thin-thumb: var(--base00) !important;
      --scrollbar-thin-track: transparent;
      --search-popout-option-fade: none;
      --search-popout-option-non-text-color: var(--base07) !important;
      --status-danger-background: var(--base08) !important;
      --status-danger: var(--base08) !important;
      --status-negative: var(--base08) !important;
      --status-positive-background: var(--base0B) !important;
      --status-positive-text: var(--base0B) !important;
      --status-positive: var(--base0B) !important;
      --status-success: var(--base0B) !important;
      --status-warning-background: var(--base03) !important;
      --status-warning: var(--base09) !important;
      --teal-430: var(--base0C) !important;
      --text-brand: var(--base07) !important;
      --text-feedback-positive: var(--base0B) !important;
      --text-feedback-negative: var(--base08) !important;
      --text-feedback-warning: var(--base09) !important;
      --text-feedback-success: var(--base0B) !important;
      --text-link: var(--base04) !important;
      --text-muted: var(--base05) !important;
      --text-negative: var(--base08) !important;
      --text-normal: var(--base05) !important;
      --text-positive: var(--base0B) !important;
      --text-primary: var(--base05) !important;
      --text-secondary: var(--base04) !important;
      --text-tertiary: var(--base03) !important;
      --text-warning: var(--base09) !important;
      --textbox-markdown-syntax: var(--base05) !important;
      --theme-base-color: var(--base00) !important;
      --white-100: var(--base05) !important;
      --white-200: var(--base05) !important;
      --white-500: var(--base05) !important;
      --white: var(--base05) !important;
      --yellow-360: var(--base0A) !important;
      --yellow-300: var(--base0A) !important;
      --__lottieIconColor: var(--base03) !important;
  }

  /*--- Default Folder Color Recolor ---*/
  .default__459fb {
      background-color: var(--base07) !important;
  }

  /*--- Add Friend Button Text Recolor ---*/
  .addFriend__133bf {
      color: var(--base00) !important;
  }

  /*--- Close Icon Path Recolor ---*/
  svg[class^="closeIcon__"] path {
      fill: var(--base01) !important;
  }

  /*--- Listen Along Invite Recolor ---*/
  .invite__4d3fa {
      background: var(--base01) !important;
      border-color: var(--base02) !important;
  }

  /*--- Activity Card Background Recolor ---*/
  .card__73069 {
      background-color: var(--base01);
  }

  div[class^="bar__"] {
      background-color: var(--base01) !important;
      border-color: var(--base02) !important;
  }
  /*--- Voice Bar Text Recolor ---*/
  .barText__7aaec {
      color: var(--base0B) !important;
  }
  .unreadIcon__7aaec {
      color: var(--base0B) !important;
  }

  /*--- Mentions Bar Text Recolor ---*/
  .mentionsBar__7aaec .barText__7aaec {
      color: var(--base05) !important;
  }

  /*--- Forum Background Recolor ---*/
  .container_f369db {
      background-color: var(--bg-overlay-2);
  }

  /*--- Sidebar Icon Recolor ---*/
  .circleIconButton__5bc7e {
      color: var(--base04);
  }

  /*--- Summaries Tag Icon Recolor ---*/
  .summariesBetaTag_cf58b5 {
      color: var(--base03);
  }

  /*--- Folder Icon Recolor ---*/
  div.folderIconWrapper__48112 {
      background-color: var(--base01) !important;
  }

  /*--- Voice Chat Action Icon Recolor ---*/
  path[fill^="rgb(88,101,242)"],
  path[stroke^="rgb(88,101,242)"] {
      fill: var(--base05) !important;
      stroke: var(--base05) !important;
  }
  .lottieIcon__5eb9b.lottieIconColors__5eb9b.buttonIcon_e131a9 {
      --__lottieIconColor: var(--base05) !important;
  }
  div[class^="actionButtons"] [class^="button"][class*="buttonColor_"],
  div[class^="actionButtons"] [class^="button"] [class*="buttonColor_"] {
      background-color: var(--base02);
  }

  /* --- Checkbox Recolor (OFF) --- */
  .container__87bf1 {
      background-color: var(--base03) !important;
  }
  /* --- Checkbox Recolor (ON) --- */
  .checked__87bf1 {
      background-color: var(--base0B) !important;
  }
  path[fill^="rgba(35, 165, 90, 1)"] {
      fill: var(--base0B) !important;
  }

  /* --- Secure Lock Icon Recolor --- */
  .lockIcon__2666b {
      display: none;
  }

  /*--- Status Icon Recolor (DO NOT DISTURB) ---*/
  svg[fill^="#f23f43"],
  rect[fill^="#f23f43"] {
      fill: var(--status-danger) !important;
  }
  /*--- Status Icon Recolor (IDLE) ---*/
  svg[fill^="#f0b232"],
  rect[fill^="#f0b232"] {
      fill: var(--status-warning) !important;
  }
  /*--- Status Icon Recolor (ONLINE) ---*/
  path[fill^="#23a55a"],
  svg[fill^="#23a55a"],
  rect[fill^="#23a55a"] {
      fill: var(--status-positive) !important;
  }
  /*--- Status Icon Recolor (OFFLINE) ---*/
  svg[fill^="#80848e"],
  rect[fill^="#80848e"] {
      fill: var(--base03) !important;
  }

  /*--- Default Color Swap ---*/
  path[fill^="currentColor"],
  svg[fill^="currentColor"],
  rect[fill^="currentColor"] {
      fill: var(--base06) !important;
  }
  path[d^="M12 22a10 10 0 1"] {
      fill: var(--base02) !important;
  }

  /*--- Voice Chat Icon Badge Recolor ---*/
  div[class^="iconBadge"] path[d^="M12 3a1 1 0 0 0-1-1h-.06"],
  div[class^="iconBadge"] path[d^="M15.16 16.51c-.57.28"] {
      fill: var(--base05) !important;
  }

  /*--- Nitro Icon Recolor ---*/
  .premiumLabel_e681d1 svg path,
  svg.guildBoostBadge__5dba5 path {
      fill: var(--base0E) !important;
  }

  /*--- Server Booster Icon Recolor ---*/
  .premiumIcon__5d473 {
      color: var(--base0F);
  }

  /*--- Call Container Recolor ---*/
  .callContainer_cb9592 {
      background-color: var(--base00);
  }
  .gradientContainer_bfe55a {
      background-image: var(--base00);
  }

  /*--- Store Gradient Recolors ---*/
  .gradient_e9ef78 {
      background: var(--base01) !important;
  }
  .bannerGradient__955a3 {
      background: var(--base00) !important;
  }

  /*--- Increase Text Legibility ---*/
  * {
      text-rendering: optimizeLegibility !important;
  }

  /*--- Codeblock Syntax Highlighting Recolor ---*/
  .hljs-attr {
      color: var(--base06) !important;
  }
  .hljs-attribute {
      color: var(--base06) !important;
  }
  .hljs-number {
      color: var(--base06) !important;
  }
  .hljs-selector-class {
      color: var(--base06) !important;
  }
  .hljs-comment {
      color: var(--base03) !important;
  }
  .hljs-subst {
      color: var(--base0D) !important;
  }
  .hljs-selector-pseudo {
      color: var(--base0B) !important;
  }
  .hljs-section {
      color: var(--base0B) !important;
  }
  .hljs-keyword {
      color: var(--base08) !important;
  }
  .hljs-variable {
      color: var(--base08) !important;
  }
  .hljs-meta {
      color: var(--base03) !important;
  }
  .hljs-built_in {
      color: var(--base09) !important;
  }
  .hljs-string {
      color: var(--base0B) !important;
  }
  .hljs-title {
      color: var(--base0E) !important;
  }

  /*--- Visual Refresh Recolor ---*/
  /*--- BIG WORK IN PROGRESS. DISCORD MADE SOME BIG CHANGES. ---*/
  .visual-refresh {
      div[class^="autocomplete__"] {
          background-color: var(--base02) !important;
      }
      path[fill^="rgba(88, 101, 242, 1)"] {
          fill: var(--base0B) !important;
      }
      div[class^="topicsPillContainer"] {
          --bg-overlay-2: var(--base02) !important;
      }
      .bg__960e4 {
          background: var(--base00) !important;
      }
      .wrapper_ef3116 {
          background-color: var(--base00) !important;
      }
      .sidebar_c48ade {
          background-color: var(--base00) !important;
      }
      .searchBar__97492 {
          background-color: var(--base02) !important;
      }
      .channelTextArea_f75fb0 {
          background: var(--base02) !important;
      }
      .chatContent_f75fb0 {
          background-color: var(--base01) !important;
      }
      .members_c8ffbb,
      .member_c8ffbb {
          background: var(--base00) !important;
      }
      .voiceBar__7aaec {
          background-color: var(--base02) !important;
      }
      button.button__67645.redGlow__67645,
      span.button__67645.redGlow__67645 {
          background-color: var(--base02) !important;
      }

      /*--- Status Icon Recolor (DO NOT DISTURB) ---*/
      svg[fill^="#d83a42"],
      rect[fill^="#d83a42"] {
          fill: var(--status-danger) !important;
      }
      /*--- Status Icon Recolor (IDLE) ---*/
      svg[fill^="#ca9654"],
      rect[fill^="#ca9654"] {
          fill: var(--status-warning) !important;
      }
      /*--- Status Icon Recolor (ONLINE) ---*/
      path[fill^="#43a25a"],
      svg[fill^="#43a25a"],
      rect[fill^="#43a25a"] {
          fill: var(--status-positive) !important;
      }
      /*--- Status Icon Recolor (OFFLINE) ---*/
      svg[fill^="#83838b"],
      rect[fill^="#83838b"] {
          fill: var(--base03) !important;
      }
  }
''
