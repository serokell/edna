// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

module.exports = {
  extends: ["stylelint-config-sass-guidelines", "stylelint-config-recess-order"],
  plugins: ["stylelint-scss", "stylelint-order"],
  rules: {
    "string-quotes": "double",
    "max-nesting-depth": 3, // block__element_modifier
    "color-named": "always-where-possible",
    "selector-class-pattern": null, // TODO camelCase regexp
    "property-no-vendor-prefix": null,
    "unit-allowed-list": ["rem", "%", "ms", "s", "vh", "vw", "turn", "fr", "em"],
    "order/properties-alphabetical-order": null,
    // "declaration-no-important": true, TODO make this rule work
  },
};
