<!--
   - SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
   -
   - SPDX-License-Identifier: CC0-1.0
   -->

As foundation for our TypeScript/SCSS styleguide
[this styleguide](https://basarat.gitbook.io/typescript/styleguide) is used,
except for the followings:

## Filename
We call files in our project according to these rules:

* Domain specific `.tsx` and `.scss`/`.css` files begin with a capital letter:
    `MyComponent.tsx`, `MainPage.tsx`, `MainPage.scss`, etc.

* For other (not domain specific, `.ts` files, assets, etc) files we use `camelCase`:
    `util.ts`, `orderedSet.ts`, `primitiveTypes.ts`, `picture.svg`, etc.

* React components and corresponding styling files are named with the same name, only file extensions are different.
