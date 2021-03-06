// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// Types used for global storage is here

import { ReactElement } from "react";
import {
  CompoundDto,
  SubExperimentDto,
  MethodologyDto,
  ParsedExcelDto,
  ProjectDto,
  MeasurementDto,
  DateTimeDto,
  SuccessSubExperimentDto,
  ResultDto,
} from "../api/types";

export interface Experiment {
  id: number;
  projectName: string;
  compoundName: string;
  targetName: string;
  methodologyName?: string;
  uploadDate: DateTimeDto;
  subExperiments: number[];
  primarySubExperimentId: number;
  primaryIC50: ResultDto<number>;
}

export type ExperimentsWithMean = {
  experiments: Experiment[];
  meanIC50?: number;
};

export type SubExperimentWithMeasurements = {
  meta: SubExperimentDto;
  measurements: MeasurementDto[];
};

export type SuccessSubExperimentWithMeasurements = {
  meta: SuccessSubExperimentDto;
  measurements: MeasurementDto[];
  target: string;
  compound: string;
};

export type FileUploadState =
  | undefined
  | { state: "uploading"; progress: number }
  | { state: "parsed"; targets: ParsedExcelDto[] }
  | { state: "adding"; targets: ParsedExcelDto[] }
  | { state: "added"; targets: ParsedExcelDto[] }
  | { state: "failed-to-add"; reason: string };

export function isParsed(
  st: FileUploadState
): st is { state: "parsed"; targets: ParsedExcelDto[]; experiments: Experiment[] } {
  return !!st && st.state === "parsed";
}

export function isAdded(st: FileUploadState): boolean {
  return !!st && (st.state === "added" || st.state === "failed-to-add");
}

export type ModalDialogState =
  | undefined
  | {
      kind: "add-edit-link";
      target:
        | { kind: "methodology"; object: MethodologyDto }
        | { kind: "compound-chemsoft"; object: CompoundDto }
        | { kind: "compound-mde"; object: CompoundDto };
    }
  | { kind: "delete-methodology"; methodology: MethodologyDto }
  | { kind: "methodology-description"; methodology: MethodologyDto }
  | { kind: "create-edit-methodology"; editing?: MethodologyDto }
  | { kind: "create-edit-project"; editing?: ProjectDto }
  | { kind: "failed-recompute-ic50"; reason: string }
  | { kind: "show-experiment-metadata"; description: string[] }
  | { kind: "rename-subexperiment"; name: string; subId: number }
  | { kind: "delete-subexperiment"; subexperiment: SubExperimentDto };

export type ExperimentsTableSize = "expanded" | "minimized";

export function negateTableSize(size: ExperimentsTableSize): ExperimentsTableSize {
  if (size === "expanded") return "minimized";
  return "expanded";
}

export const chartColors = [
  "#4285F4",
  "#0B8043",
  "#D50000",
  "#F6BF26",
  "#8E24AA",
  "#3F51B5",
  "#F4511E",
  "#33B679",
  "#7986CB",
  "#E67C73",
];

export type NewSubExperiment = {
  subExperimentId: number;
  changedPoints: MeasurementDto[];
  // Result of 4PL
  analysed?: number[];
};

// Notifications

export type NotificationType = "Success" | "Error" | "Warn";

export interface DeleteNotification {
  type: "Delete";
  id: number;
}

export interface AddNotification {
  type: "Add";
  notificationType: NotificationType;
  element: (manualRemove: () => void) => ReactElement;
}

export type NotificationUpdateAction = DeleteNotification | AddNotification;

export interface NotificationData {
  id: number;
  element: (manualRemove: () => void) => ReactElement;
  type: NotificationType;
}

export interface Notifications {
  lastId: number;
  notifications: NotificationData[];
}
