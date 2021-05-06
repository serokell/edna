// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// For testing and mocking purposes
import { RefObject, useEffect } from "react";
import { DateTimeDto, ResultDto, singleResultDto } from "../api/types";

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

export function formatAsDate(t: DateTimeDto): string {
  const options: Intl.DateTimeFormatOptions = {
    year: "numeric",
    month: "numeric",
    day: "numeric",
  };
  return new Date(Date.parse(t)).toLocaleDateString("ru-RU", options);
}

export function formatAsDateTime(t: DateTimeDto): string {
  const options: Intl.DateTimeFormatOptions = {
    year: "numeric",
    month: "numeric",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
  };
  return new Date(Date.parse(t)).toLocaleDateString("ru-RU", options);
}

export function replaceEmptyWithUndefined(x: string): Maybe<string> {
  return x || undefined;
}

export function zip<A, B>(a: A[], b: B[]): [A, B][] {
  return a.map((e, i) => [e, b[i]]);
}

// We show at most 5 characters. We empirically determined that 5 characters
// can be fit into the IC50 column.
// If we return a string that doesn't fit into the column for some reason,
// we have fallback logic in CSS that limits the width and adds ellipsis.
export function formatIC50(x: number): string {
  // We are carefully listing all cases here to think about each of them.
  // Perhaps it can be optimized, e. g. using 'toPrecision', but I (@gromak)
  // decided to follow the KISS principle.
  switch (true) {
    // More than 5 digits in total.
    // We show it in exponential notation with just one significant digit.
    // Usually such large values mean that the model is bad and the exact value
    // doesn't matter.
    case x >= 1e5:
      return x.toExponential(0);

    // 5 or 4 digits in total, no digits after decimal point.
    case x >= 1e3:
      return x.toFixed(0);

    // 3 digits before the decimal point, 1 after.
    case x >= 1e2:
      return x.toFixed(1);

    // 2 digits before the decimal point, 2 after.
    case x >= 10:
      return x.toFixed(2);

    // 1 digit before the decimal point, 3 after.
    case x >= 1:
      return x.toFixed(3);

    // '0' before the decimal point, 3 digits after.
    case x >= 1e-4:
      return x.toFixed(3);

    // Very small value, just show exponential format with one significat digit.
    default:
      return x.toExponential(0);
  }
}

export function useClickOutsideCallback(ref: RefObject<HTMLElement>, onOutside: () => void): void {
  useEffect(() => {
    const handleClickOutside = (e: MouseEvent) => {
      if (ref.current && !ref.current.contains(e.target as any)) {
        onOutside();
      }
    };

    // Bind the event listener
    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      // Unbind the event listener on clean up
      document.removeEventListener("mousedown", handleClickOutside);
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [ref]);
}

export function linspace(start: number, stop: number, len: number): number[] {
  const step = (stop - start) / (len - 1);
  return Array.from({ length: len }, (_, i) => start + step * i);
}

export function logspace(start: number, stop: number, len: number): number[] {
  const startPoint = Math.log10(start);
  const endPoint = Math.log10(stop);

  const linPoints = linspace(startPoint, endPoint, len);

  return Array.from(linPoints, (p: number) => 10 ** p);
}

export function evalMeanIC50(elems: ResultDto<number | number[]>[]): ResultDto<number> {
  const ic50Sum = elems.reduce(
    (acc: ResultDto<number>, x) => {
      const res = singleResultDto(x);
      return "Left" in acc
        ? acc
        : "Left" in res
        ? { Left: res.Left }
        : { Right: acc.Right + res.Right };
    },
    { Right: 0 }
  );
  if ("Left" in ic50Sum) return ic50Sum;
  return { Right: ic50Sum.Right / elems.length };
}
