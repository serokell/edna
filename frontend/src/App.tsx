// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, ReactElement, useEffect } from "react";
import { Route, Switch, useLocation } from "react-router-dom";
import { Redirect } from "react-router";

import { useRecoilCallback, useRecoilValue, useResetRecoilState, useSetRecoilState } from "recoil";
import { UploadPage } from "./pages/upload/UploadPage";
import { LibraryPage } from "./pages/library/LibraryPage";
import { NotFound } from "./components/NotFound/NotFound";
import {
  colorsCounterAtom,
  compoundIdSelectedAtom,
  excelFileAtom,
  experimentsTableSizeAtom,
  modalDialogAtom,
  newSubexperimentAtom,
  projectSelectedIdAtom,
  selectedSubExperimentsColorAtom,
  selectedSubExperimentsIdsAtom,
  targetIdSelectedAtom,
} from "./store/atoms";
import { CreateMethodologyDialog } from "./components/dialogs/CreateMethodologyDialog";
import { CreateProjectDialog } from "./components/dialogs/CreateProjectDialog";
import { MethodologyDescriptionDialog } from "./components/dialogs/MethodologyDescriptionDialog";
import { DeleteMethodologyDialog } from "./components/dialogs/DeleteMethodologyDialog";
import { AddLinkDialog } from "./components/dialogs/AddLinkDialog";
import { DashboardPage } from "./pages/dashboard/DashboardPage";
import { chartColors } from "./store/types";
import { SimpleDialog } from "./components/dialogs/SimpleDialog";
import { RenameSubexperimentDialog } from "./components/dialogs/RenameSubexperimentDialog";
import { DeleteSubexperimentDialog } from "./components/dialogs/DeleteSubexperimentDialog";

export const App: FunctionComponent = (): ReactElement => {
  const modalDialog = useRecoilValue(modalDialogAtom);
  const location = useLocation();
  const resetExcelFile = useResetRecoilState(excelFileAtom);
  const resetProjectSelectedId = useResetRecoilState(projectSelectedIdAtom);
  const resetCompoundIdSelected = useResetRecoilState(compoundIdSelectedAtom);
  const resetTargetIdSelected = useResetRecoilState(targetIdSelectedAtom);
  const resetExperimentsTableSizeAtom = useResetRecoilState(experimentsTableSizeAtom);
  const resetNewSubExperiment = useResetRecoilState(newSubexperimentAtom);
  const setSelectedSubExperimentsIds = useSetRecoilState(selectedSubExperimentsIdsAtom);
  const selectedSubExperiments = useRecoilValue(selectedSubExperimentsIdsAtom);

  const resetColorsCounter = useRecoilCallback(({ reset }) => () => {
    for (let i = 0; i < chartColors.length; i++) {
      reset(colorsCounterAtom(chartColors[i]));
    }
  });

  const resetSubexperimentsColors = useRecoilCallback(({ reset }) => () => {
    const arr = Array.from(selectedSubExperiments);
    for (let i = 0; i < arr.length; i++) {
      reset(selectedSubExperimentsColorAtom(arr[i]));
    }
  });

  useEffect(() => {
    // Cleaners of state when we leave a page
    if (location.pathname !== "/upload") {
      resetExcelFile();
    }

    if (location.pathname !== "/dashboard") {
      resetColorsCounter();
      resetSubexperimentsColors();
      resetProjectSelectedId();
      resetCompoundIdSelected();
      resetTargetIdSelected();
      resetNewSubExperiment();
      resetExperimentsTableSizeAtom();
      // For some weird reason reset doesn't update selectedExperimentsQuery selector
      setSelectedSubExperimentsIds(new Set<number>());
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [location.pathname]);

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
      {modalDialog?.kind === "failed-recompute-ic50" && (
        <SimpleDialog title="Failed to recompute IC50" description={modalDialog.reason} />
      )}
      {modalDialog?.kind === "show-experiment-metadata" && (
        <SimpleDialog title="Metadata" description={modalDialog.description} btnText="Close" />
      )}
      {modalDialog?.kind === "rename-subexperiment" && (
        <RenameSubexperimentDialog name={modalDialog.name} subId={modalDialog.subId} />
      )}
      {modalDialog?.kind === "delete-subexperiment" && (
        <DeleteSubexperimentDialog subexperiment={modalDialog.subexperiment} />
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
          <NotFound page />
        </Route>
      </Switch>
    </>
  );
};
