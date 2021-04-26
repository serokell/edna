// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// The module might be removed if we start using open api client generator

// Data transfer object
interface Dto<T, Extra = undefined> {
  id: number;
  item: T;
  extra?: Extra;
}

type ParsedCompoundDto = {
  id?: number;
  name: string;
};

type ParsedTargetDto = {
  id?: number;
  name: string;
};

export type ParsedExcelDto = {
  target: ParsedTargetDto;
  compounds: ParsedCompoundDto[];
};

export type ResultDto =
  | {
      Left: string;
    }
  | {
      Right: number[];
    };

export type SubExperimentDto = Dto<{
  name: string;
  isSuspicious: boolean;
  result: ResultDto;
}>;

export type SuccessSubExperimentDto = Dto<{
  name: string;
  isSuspicious: boolean;
  result: { Right: number[] };
}>;

export type MeasurementDto = Dto<{
  concentration: number;
  signal: number;
  isEnabled: boolean;
}>;

interface ProjectBodyDto {
  name: string;
  description?: string;
  creationDate: DateTimeDto;
  lastUpdate: DateTimeDto;
  compoundNames: string[];
}

export type DateTimeDto = string;

export type ProjectDto = Dto<ProjectBodyDto>;

interface MethodologyBodyDto {
  name: string;
  description?: string;
  confluence?: string;
  projects: string[];
}

export type MethodologyDto = Dto<MethodologyBodyDto>;

export type TargetDto = {
  id: number;
  item: {
    name: string;
    projects: string[];
    additionDate: DateTimeDto;
  };
};

export type CompoundDto = {
  id: number;
  item: {
    name: string;
    chemSoft?: string;
    additionDate: DateTimeDto;
  };
};

export type ExperimentDto = Dto<{
  project: number;
  compound: number;
  target: number;
  methodology?: number;
  uploadDate: DateTimeDto;
  subExperiments: number[];
  primarySubExperiment: number;
}>;

export type ExperimentsWithMeanDto = {
  experiments: ExperimentDto[];
  meanIC50?: number;
};

export type ExperimentMetadataDto = {
  description: string;
  fileMetadata: string[];
};
