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
