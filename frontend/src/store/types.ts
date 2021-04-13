// Types used for global storage is here

import {
  CompoundDto,
  SubExperimentDto,
  MethodologyDto,
  ParsedExcelDto,
  ProjectDto,
  MeasurementDto,
  DateTimeDto,
} from "../api/types";

export interface Experiment {
  id: number;
  projectName: string;
  compoundName: string;
  targetName: string;
  methodologyName?: string;
  uploadDate: DateTimeDto;
  subExperiments: number[];
  primarySubExperiment: SubExperimentDto;
}

export type ExperimentsWithMean = {
  experiments: Experiment[];
  meanIC50?: number;
};

export type SubExperimentWithMeasurements = {
  meta: SubExperimentDto;
  measurements: MeasurementDto[];
};

export type FileUploadState =
  | undefined
  | { state: "uploading"; progress: number }
  | { state: "verifying" }
  | { state: "parsed"; targets: ParsedExcelDto[] }
  | { state: "failed-to-parse"; reason: string }
  | { state: "added"; targets: ParsedExcelDto[] }
  | { state: "failed-to-add"; reason: string };

export function isParsed(
  st: FileUploadState
): st is { state: "parsed"; targets: ParsedExcelDto[]; experiments: Experiment[] } {
  return !!st && st.state === "parsed";
}

export type ModalDialogState =
  | undefined
  | {
      kind: "add-edit-link";
      target:
        | { kind: "methodology"; object: MethodologyDto }
        | { kind: "compound"; object: CompoundDto };
    }
  | { kind: "delete-methodology"; methodology: MethodologyDto }
  | { kind: "methodology-description"; methodology: MethodologyDto }
  | { kind: "create-edit-methodology"; editing?: MethodologyDto }
  | { kind: "create-edit-project"; editing?: ProjectDto };

export type ExperimentsTableSize = "expanded" | "minimized";

export function negateTableSize(size: ExperimentsTableSize): ExperimentsTableSize {
  if (size === "expanded") return "minimized";
  return "expanded";
}

export const chartColors = [
  "#C6E294",
  "#8E95D5",
  "#E6D85D",
  "#3D9C97",
  "#C15959",
  "#53AC62",
  "#A26BC4",
  "#6076E0",
  "#374275",
  "#BF5688",
];

export type NewSubExperiment = {
  subExperimentId: number;
  changedPoints: MeasurementDto[];
  analysed?: {
    // Result of 4PL
    result: number[];
  };
};
