// Basic state should be here
import { atom, selector } from "recoil";
import { FileUploadState, Methodology, ModalDialogState } from "./types";
import Api from "../api/api";
import { CompoundDto, ProjectDto, TargetDto } from "../api/types";

export const projectsAtom = atom<ProjectDto[]>({
  key: "Projects",
  default: selector({
    key: "Projects/Default",
    get: () => Api.fetchProjects(),
  }),
});

export const targetsAtom = atom<TargetDto[]>({
  key: "Targets",
  default: selector({
    key: "Targets/Default",
    get: () => Api.fetchTargets(),
  }),
});

export const compoundsAtom = atom<CompoundDto[]>({
  key: "Compounds",
  default: selector({
    key: "Compounds/Default",
    get: () => Api.fetchCompounds(),
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
