// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// For testing and mocking purposes
import { RefObject, useEffect } from "react";
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

export function formatIC50(x: number): string {
  return x.toFixed(3);
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
