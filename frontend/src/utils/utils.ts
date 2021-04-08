// For testing and mocking purposes
import { DateTimeDto } from "../api/types";

export function delay(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export type Maybe<T> = T | undefined;

export function isDefined<T>(anyVal: T | undefined | null): anyVal is T {
  return anyVal !== undefined && anyVal !== null;
}

export function capitalizeFirstLetter(s: string): string {
  return s.charAt(0).toUpperCase() + s.slice(1);
}

export function formatDateTimeDto(t: DateTimeDto): string {
  const options: Intl.DateTimeFormatOptions = {
    year: "numeric",
    month: "numeric",
    day: "numeric",
  };
  return new Date(Date.parse(t)).toLocaleDateString("ru-RU", options);
}

export function replaceEmptyWithUndefined(x: string): Maybe<string> {
  return x || undefined;
}

export function extraFormatter(items: string[]): string {
  if (items.length <= 4) return items.join(", ");
  return `${items.slice(0, 4).join(", ")} and ${items.length - 4} more`;
}

export function zip<A, B>(a: A[], b: B[]): [A, B][] {
  return a.map((e, i) => [e, b[i]]);
}
