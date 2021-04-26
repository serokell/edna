// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { Maybe } from "../../utils/utils";
import { UploadExperimentsArgsApi } from "../../api/EdnaApi";
import { MethodologyDto, ProjectDto } from "../../api/types";

export interface UploadForm {
  file: Maybe<File>;
  methodology: Maybe<MethodologyDto>;
  project: Maybe<ProjectDto>;
  description: string;
}

export function uploadFormToApi(form: UploadForm): Maybe<UploadExperimentsArgsApi> {
  if (form.file && form.project && form.methodology) {
    return {
      projectId: form.project.id,
      methodologyId: form.methodology.id,
      file: form.file,
      description: form.description,
    };
  }
  return undefined;
}
