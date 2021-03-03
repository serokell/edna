// RequestState machinery
// This file might be removed if we start using redux-query or recoil

export type RequestState<T> =
  | { status: "idle" }
  | { status: "loading" }
  | { status: "succeeded"; result: T }
  | { status: "failed"; error?: string };

type SucceededRequest<T> = {
  status: "succeeded";
  result: T;
};

export function idle<T>(): RequestState<T> {
  return { status: "idle" };
}

export function loading<T>(): RequestState<T> {
  return { status: "loading" };
}

export function completed<T>(x: T): RequestState<T> {
  return { status: "succeeded", result: x };
}

export function failed(x?: string): RequestState<any> {
  return { status: "failed", error: x };
}

export function isSucceeded<T>(x: RequestState<T>): x is SucceededRequest<T> {
  return x.status === "succeeded";
}
