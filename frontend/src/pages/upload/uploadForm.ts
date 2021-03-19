import { Maybe } from "../../utils/utils";
import { Methodology } from "../../store/types";
import { UploadExperimentsArgs } from "../../api/EdnaApi";
import { ProjectDto } from "../../api/types";

export interface UploadForm {
  file: Maybe<File>;
  methodology: Maybe<Methodology>;
  project: Maybe<ProjectDto>;
  description: string;
}

export function uploadFormToApi(form: UploadForm): Maybe<UploadExperimentsArgs> {
  if (form.file && form.project && form.methodology) {
    return {
      projectId: form.project.id,
      methodologyId: form.methodology.methodologyId,
      file: form.file,
      description: form.description,
    };
  }
  return undefined;
}
