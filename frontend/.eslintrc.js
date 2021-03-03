module.exports = {
  extends: [
    "airbnb-typescript",
    "airbnb/hooks",
    "plugin:@typescript-eslint/recommended",
    "plugin:prettier/recommended",
    "prettier/@typescript-eslint",
    "prettier/react",
  ],

  parserOptions: {
    project: ["./tsconfig.json"],
  },

  ignorePatterns: ["/*.*"],

  plugins: ["@typescript-eslint"],
  parser: "@typescript-eslint/parser",
  rules: {
    // Sometime we need to specify any type, so it's rather useful
    "@typescript-eslint/no-explicit-any": 0,

    "react/jsx-closing-bracket-location": 0,

    // Import modules from different places
    "import/no-unresolved": 0,

    // JSX extensions support
    "react/jsx-filename-extension": [
      2,
      {
        extensions: [".jsx", ".tsx"],
      },
    ],

    quotes: "off",
    "@typescript-eslint/quotes": [2, "double", { avoidEscape: true }],
    'no-plusplus': [2, { allowForLoopAfterthoughts: true }],

    "@typescript-eslint/comma-dangle":
      ['error', {
        arrays: 'only-multiline',
        objects: 'only-multiline',
        imports: 'only-multiline',
        exports: 'only-multiline',
        functions: 'never', },],

    // Use TS no-unused-vars rule
    "no-unused-vars": "off",
    "@typescript-eslint/no-unused-vars": ["error"],
    "@typescript-eslint/explicit-module-boundary-types": 0,

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
    "no-constant-condition": ["error", { checkLoops: false }],

    "react/jsx-props-no-spreading": 0,
    "jsx-a11y/tabindex-no-positive": 0
  },
  globals: {
    window: true,
    alert: true,
    document: true,
    Node: true,
  },
};
