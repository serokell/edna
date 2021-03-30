import { MethodologyDto, ProjectDto } from "../../api/types";

export interface CreateProjectForm {
  name: string;
  description: string;
}

export function toCreateProjectForm(proj?: ProjectDto): CreateProjectForm {
  return {
    name: proj?.item.name ?? "",
    description: proj?.item.description ?? "",
  };
}

export interface CreateMethodologyForm {
  name: string;
  description: string;
  confluence: string;
}

export function toCreateMethodologyForm(meth?: MethodologyDto): CreateMethodologyForm {
  return {
    name: meth?.item.name ?? "",
    description: meth?.item.description ?? "",
    confluence: meth?.item.confluence ?? "",
  };
}
