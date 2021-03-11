// Basic state should be here
import { atom, RecoilState, selector } from "recoil";
import { Methodology, Project } from "./types";
import Api from "../api/api";

export const projectsAtom: RecoilState<Project[]> = atom({
  key: "Projects",
  default: selector({
    key: "Projects/Default",
    get: () => Api.fetchProjects(),
  }),
});

export const methodologiesAtom: RecoilState<Methodology[]> = atom({
  key: "Methodologies",
  default: selector({
    key: "Methodologies/Default",
    get: () => Api.fetchMethodologies(),
  }),
});
