// eslint-disable-next-line import/prefer-default-export
export function isDefined<T>(anyVal: T | undefined | null): anyVal is T {
  return anyVal !== undefined && anyVal !== null;
}
