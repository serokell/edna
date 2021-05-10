// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// Basic state should be here
import { atom, atomFamily } from "recoil";
import {
  NewSubExperiment,
  ExperimentsTableSize,
  FileUploadState,
  ModalDialogState,
  Notifications,
} from "./types";
import { Maybe } from "../utils/utils";
import Api from "../api/api";
import { ExperimentMetadataDto, MeasurementDto, SubExperimentDto } from "../api/types";

// Global
export const modalDialogAtom = atom<ModalDialogState>({
  key: "ModalDialogState",
  default: undefined,
});

// Library page
export const projectsRequestIdAtom = atom<number>({
  key: "ProjectsReqId",
  default: 0,
});

export const targetsRequestIdAtom = atom<number>({
  key: "TargetsReqId",
  default: 0,
});

export const compoundsReqIdAtom = atom<number>({
  key: "CompoundsReqId",
  default: 0,
});

export const methodologiesRequestIdAtom = atom<number>({
  key: "MethodologiesReqId",
  default: 0,
});

// Upload page
export const excelFileAtom = atom<FileUploadState>({
  key: "ExcelFileToUpload",
  default: undefined,
});

// Dashboard page
export const filteredExperimentsReqIdAtom = atom<number>({
  key: "filteredExperimentsReqId",
  default: 0,
});

export const projectSelectedIdAtom = atom<Maybe<number>>({
  key: "DashboardProjectIdSelected",
  default: undefined,
});

export const compoundIdSelectedAtom = atom<Maybe<number>>({
  key: "DashboardCompoundIdSelected",
  default: undefined,
});

export const targetIdSelectedAtom = atom<Maybe<number>>({
  key: "DashboardTargetIdSelected",
  default: undefined,
});

export const experimentsTableSizeAtom = atom<ExperimentsTableSize>({
  key: "ExperimentsTableSize",
  default: "minimized",
});

export const selectedSubExperimentsIdsAtom = atom<Set<number>>({
  key: "SelectedSubExperimentsIds",
  default: new Set<number>(),
});

export const subExperimentsMetaMap = atomFamily<SubExperimentDto, number>({
  key: "SubExperimentsMeta",
  default: async subExperimentId => {
    return Api.fetchSubExperiment(subExperimentId);
  },
});

export const subExperimentsMeasurementsMap = atomFamily<MeasurementDto[], number>({
  key: "SubExperimentsMeasurements",
  default: async subExperimentId => {
    return Api.fetchMeasurements(subExperimentId);
  },
});

export const colorsCounterAtom = atomFamily<number, string>({
  key: "SelectedSubExperimentsColors",
  default: 0,
});

export const selectedSubExperimentsColorAtom = atomFamily<Maybe<string>, number>({
  key: "SelectedSubExperimentsColor",
  default: undefined,
});

export const newSubexperimentAtom = atom<NewSubExperiment>({
  key: "NewSubExperiment",
  default: {
    changedPoints: [],
    subExperimentId: -1,
  },
});

export const experimentMetadata = atomFamily<ExperimentMetadataDto, number>({
  key: "ExperimentMetadata",
  default: async experimentId => {
    return Api.fetchExperimentMetadata(experimentId);
  },
});

// Notifications

export const notificationListAtom = atom<Notifications>({
  key: "NotificationList",
  default: { lastId: 0, notifications: [] },
});
