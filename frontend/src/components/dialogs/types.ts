// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { MethodologyDto, ProjectDto } from "../../api/types";

// Create Project
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

// Create Methodology

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

// Add link dialog

export interface AddLinkForm {
  link: string;
}

// Rename subexperiment dialog

export interface RenameSubexperiment {
  name: string;
}
