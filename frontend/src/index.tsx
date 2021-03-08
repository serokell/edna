import ReactDOM from "react-dom";
import { BrowserRouter as Router } from "react-router-dom";
import React from "react";
import { RecoilRoot } from "recoil";
import { App } from "./App";
import "./index.scss";

ReactDOM.render(
  <Router>
    <RecoilRoot>
      <App />
    </RecoilRoot>
  </Router>,
  document.getElementById("root")
);
