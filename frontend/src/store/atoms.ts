// Basic state should be here
import { atom, selector } from "recoil";
import { FileUploadState, Methodology, ModalDialogState, Project } from "./types";
import Api from "../api/api";

export const projectsAtom = atom<Project[]>({
  key: "Projects",
  default: selector({
    key: "Projects/Default",
    get: () => Api.fetchProjects(),
  }),
});

export const methodologiesAtom = atom<Methodology[]>({
  key: "Methodologies",
  default: selector({
    key: "Methodologies/Default",
    get: () => Api.fetchMethodologies(),
  }),
});

export const excelFileAtom = atom<FileUploadState>({
  key: "ExcelFileToUpload",
  default: undefined,
});

export const modalDialogAtom = atom<ModalDialogState>({
  key: "ModalDialogState",
  default: undefined,
});
