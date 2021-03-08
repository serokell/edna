import React, { FunctionComponent, ReactElement } from "react";
import { Route, Switch } from "react-router-dom";
import { Redirect } from "react-router";

import { UploadPage } from "./pages/upload/UploadPage";
import { LibraryPage } from "./pages/library/LibraryPage";
import { NotFound } from "./components/NotFound/NotFound";

export const App: FunctionComponent = (): ReactElement => {
  return (
    <Switch>
      <Route path="/upload">
        <UploadPage />
      </Route>
      <Route path="/library">
        <LibraryPage />
      </Route>

      <Redirect exact from="/" to="/upload" />

      <Route path="*">
        <NotFound />
      </Route>
    </Switch>
  );
};
