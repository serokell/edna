// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// To allow import of assets directly from the tsx files
// without tsc type error

declare module "*.svg?inline" {
  const content: any;
  export default content;
}

declare module "*.svg" {
  const content: any;
  export default content;
}

declare module "*.png?inline" {
  const content: any;
  export default content;
}

declare module "*.png" {
  const content: any;
  export default content;
}
