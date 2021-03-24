// Basic state should be here
import { atom } from "recoil";
import { FileUploadState, ModalDialogState } from "./types";

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

export const excelFileAtom = atom<FileUploadState>({
  key: "ExcelFileToUpload",
  default: undefined,
});

export const modalDialogAtom = atom<ModalDialogState>({
  key: "ModalDialogState",
  default: undefined,
});
