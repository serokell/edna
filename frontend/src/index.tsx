import ReactDOM from "react-dom";
import React from "react";
import { RecoilRoot } from "recoil";
import App from "./App";
import "./index.scss";

ReactDOM.render(
  <RecoilRoot>
    <App />
  </RecoilRoot>,
  document.getElementById("root")
);
