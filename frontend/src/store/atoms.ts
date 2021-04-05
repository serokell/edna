// Basic state should be here
import { atom } from "recoil";
import { FileUploadState, ModalDialogState } from "./types";
import { Maybe } from "../utils/utils";

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
