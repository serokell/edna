module.exports = {
  extends: [
    "react-app",
    "react-app/jest",
    "plugin:@typescript-eslint/recommended",
    "plugin:import/typescript",
    "plugin:prettier/recommended",
    "prettier/@typescript-eslint",
    "prettier/react"
  ],

  plugins: ["@typescript-eslint"],
  parser: "@typescript-eslint/parser",
  rules: {
    // Sometime we need to specify any type, so it's rather useful
    "@typescript-eslint/no-explicit-any": 0,

    // Import modules from different places
    "import/no-unresolved": 0,

    // JSX extensions support
    "react/jsx-filename-extension": [
      2,
      {
        extensions: [".jsx", ".tsx"]
      }
    ],

    // Use double quotes
    quotes: [2, "double", { avoidEscape: true }],

    // Use TS no-unused-vars rule
    "no-unused-vars": "off",
    "@typescript-eslint/no-unused-vars": ["error"],

    // Support declarations in case
    "no-case-declarations": 0,

    // We already write props type in generic of functional component
    "react/prop-types": 0,

    "no-alert": 0, // TODO enable it later

    // Rules for making interface available for people with disabilities
    "jsx-a11y/control-has-associated-label": 0,
    "jsx-a11y/click-events-have-key-events": 0,
    "jsx-a11y/no-static-element-interactions": 0,

    // Enable while(true) loops
    "no-constant-condition": ["error", { checkLoops: false }]
  },
  globals: {
    window: true,
    alert: true,
    document: true,
    Node: true
  }
};