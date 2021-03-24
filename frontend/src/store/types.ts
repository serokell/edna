// Types used for global storage is here

import { MeasurementDto, ParsedExcelDto } from "../api/types";

export interface Experiment {
  target: string;
  compoundId: string;
  measurements: MeasurementDto[];
}

export type FileUploadState =
  | undefined
  | { state: "uploading"; progress: number }
  | { state: "verifying" }
  | { state: "parsed"; targets: ParsedExcelDto[]; experiments: Experiment[] }
  | { state: "failed-to-parse"; reason: string }
  | { state: "added"; targets: ParsedExcelDto[] }
  | { state: "failed-to-add"; reason: string };

export function isParsed(
  st: FileUploadState
): st is { state: "parsed"; targets: ParsedExcelDto[]; experiments: Experiment[] } {
  return !!st && st.state === "parsed";
}

export type ModalDialogState = undefined | "create-methodology" | "create-project";
