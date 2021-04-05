import React, { FunctionComponent, ReactElement } from "react";
import { Route, Switch } from "react-router-dom";
import { Redirect } from "react-router";

import { useRecoilValue } from "recoil";
import { UploadPage } from "./pages/upload/UploadPage";
import { LibraryPage } from "./pages/library/LibraryPage";
import { NotFound } from "./components/NotFound/NotFound";
import { modalDialogAtom } from "./store/atoms";
import { CreateMethodologyDialog } from "./components/dialogs/CreateMethodologyDialog";
import { CreateProjectDialog } from "./components/dialogs/CreateProjectDialog";
import { MethodologyDescriptionDialog } from "./components/dialogs/MethodologyDescriptionDialog";
import { DeleteMethodologyDialog } from "./components/dialogs/DeleteMethodologyDialog";
import { AddLinkDialog } from "./components/dialogs/AddLinkDialog";
import { DashboardPage } from "./pages/dashboard/DashboardPage";

export const App: FunctionComponent = (): ReactElement => {
  const modalDialog = useRecoilValue(modalDialogAtom);

  return (
    <>
      {modalDialog?.kind === "add-edit-link" && <AddLinkDialog target={modalDialog.target} />}

      {modalDialog?.kind === "delete-methodology" && (
        <DeleteMethodologyDialog methodology={modalDialog.methodology} />
      )}

      {modalDialog?.kind === "methodology-description" && (
        <MethodologyDescriptionDialog methodology={modalDialog.methodology} />
      )}

      {modalDialog?.kind === "create-edit-methodology" && (
        <CreateMethodologyDialog editing={modalDialog.editing} />
      )}
      {modalDialog?.kind === "create-edit-project" && (
        <CreateProjectDialog editing={modalDialog.editing} />
      )}
      <Switch>
        <Route path="/upload">
          <UploadPage />
        </Route>
        <Route path="/library">
          <LibraryPage />
        </Route>

        <Route path="/dashboard">
          <DashboardPage />
        </Route>

        <Redirect exact from="/" to="/upload" />

        <Route path="*">
          <NotFound />
        </Route>
      </Switch>
    </>
  );
};
