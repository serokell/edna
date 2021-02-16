import React, { FunctionComponent, ReactElement } from "react";
import SerokellLogo from "~/assets/svg/serokell.svg";

// interface F {
//   x: number;
//   y: number;
// }

export type Z = "a" | "b";

export const App: FunctionComponent = (): ReactElement => (
  // Uncomment this code make sure eslint react config works
  // const f = () => {
  //   useEffect(() => {
  //     return () => {
  //       // do nothing intentionally
  //     };
  //   }, []);
  // };
  // f();

  // const l = 4;
  // const fun = (x: number) => {
  //   return x + 5;
  // };

  <>
    <SerokellLogo />
    Hello, Gromak! How are you?
  </>
);
