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
  methodologyName: string;
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
