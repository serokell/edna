import { Maybe } from "../../utils/utils";
import { Methodology, Project } from "../../store/types";
import { UploadExperimentsArgs } from "../../api/EdnaApi";

export interface UploadForm {
  file: Maybe<File>;
  methodology: Maybe<Methodology>;
  project: Maybe<Project>;
  description: string;
}

export function uploadFormToApi(form: UploadForm): Maybe<UploadExperimentsArgs> {
  if (form.file && form.project && form.methodology) {
    return {
      projectId: form.project.projectId,
      methodologyId: form.methodology.methodologyId,
      file: form.file,
      description: form.description,
    };
  }
  return undefined;
}
