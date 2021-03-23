// Types used for global storage is here

import { MeasurementDto, ParsedTargetDto } from "../api/types";

export interface Project {
  projectId: number;
  name: string;
  description?: string;
}

export interface Methodology {
  methodologyId: number;
  name: string;
}

export interface Experiment {
  target: string;
  compoundId: string;
  measurements: MeasurementDto[];
}

export type FileUploadState =
  | undefined
  | { state: "uploading"; progress: number }
  | { state: "verifying" }
  | { state: "parsed"; targets: ParsedTargetDto[]; experiments: Experiment[] }
  | { state: "failed-to-parse"; reason: string }
  | { state: "added" }
  | { state: "failed-to-add"; reason: string };

export function isParsed(
  st: FileUploadState
): st is { state: "parsed"; targets: ParsedTargetDto[]; experiments: Experiment[] } {
  return !!st && st.state === "parsed";
}

export type ModalDialogState = undefined | "create-methodology" | "create-project";
