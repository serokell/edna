import ReactDOM from "react-dom";
import { BrowserRouter as Router } from "react-router-dom";
import React from "react";
import { RecoilRoot } from "recoil";
import "./index.scss";
import { App } from "./App";

ReactDOM.render(
  <Router>
    <RecoilRoot>
      <App />
    </RecoilRoot>
  </Router>,
  document.getElementById("root")
);
