import React, {FunctionComponent, ReactElement} from "react";
import SerokellLogo from "~/assets/serokell.svg"

export const App: FunctionComponent = (): ReactElement => {
  // TODO remove. Code needed to check eslint react config
  // const f = () => {
  //     useEffect(() => {
  //
  //     }, [])
  // }
  // f()
  return <>
    <SerokellLogo/>
    Hello, Gromak! How are you?'</>
}
