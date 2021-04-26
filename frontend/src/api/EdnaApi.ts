// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// The module might be removed if we start using open api client generator
import { AxiosInstance, AxiosResponse } from "axios";
import {
  CompoundDto,
  ExperimentsWithMeanDto,
  MeasurementDto,
  ExperimentMetadataDto,
  MethodologyDto,
  ParsedExcelDto,
  ProjectDto,
  ResultDto,
  SubExperimentDto,
  TargetDto,
} from "./types";
import { isDefined, Maybe, replaceEmptyWithUndefined } from "../utils/utils";
import { CreateMethodologyForm, CreateProjectForm } from "../components/dialogs/types";

export interface UploadExperimentsArgsApi {
  file: File;
  projectId: number;
  methodologyId: number;
  description: Maybe<string>;
}

export interface CreateMethodologyArgsApi {
  name: string;
  description: Maybe<string>;
  confluence: Maybe<string>;
}

export function toCreateMethodologyArgsApi(form: CreateMethodologyForm): CreateMethodologyArgsApi {
  return {
    name: form.name,
    description: replaceEmptyWithUndefined(form.description.trim()),
    confluence: replaceEmptyWithUndefined(form.confluence.trim()),
  };
}

export interface CreateProjectArgsApi {
  name: string;
  description: Maybe<string>;
}

export interface AnalyzeNewSubexperimentApi {
  name: string;
  changes: number[];
}

export type SortParamsApi = {
  sortby?: string;
  desc?: boolean;
};

export function toCreateProjectArgsApi(form: CreateProjectForm): CreateProjectArgsApi {
  return {
    name: form.name,
    description: replaceEmptyWithUndefined(form.description.trim()),
  };
}

interface EdnaApiInterface {
  parseExcelFile: (
    excelFile: Blob,
    onUploadProgress: (percent: number) => void
  ) => Promise<ParsedExcelDto[]>;

  uploadExperiments(form: UploadExperimentsArgsApi): Promise<ParsedExcelDto[]>;

  fetchProjects: (params: SortParamsApi) => Promise<ProjectDto[]>;
  createProject: (args: CreateProjectArgsApi) => Promise<ProjectDto>;
  editProject: (projId: number, args: CreateProjectArgsApi) => Promise<ProjectDto>;
  fetchTargets: (params: SortParamsApi) => Promise<TargetDto[]>;
  fetchCompounds: (params: SortParamsApi) => Promise<CompoundDto[]>;
  updateChemSoftLink: (compoundId: number, newLink: string) => Promise<any>;
  updateMdeLink: (compoundId: number, newLink: string) => Promise<any>;
  fetchMethodologies: (params: SortParamsApi) => Promise<MethodologyDto[]>;
  createMethodology: (args: CreateMethodologyArgsApi) => Promise<MethodologyDto>;
  editMethodology: (methId: number, args: CreateMethodologyArgsApi) => Promise<MethodologyDto>;
  deleteMethodology: (methId: number) => Promise<any>;

  fetchExperiments: (
    projectId?: number,
    compoundId?: number,
    targetId?: number,
    params?: SortParamsApi
  ) => Promise<ExperimentsWithMeanDto>;
  fetchSubExperiment: (subExperimentId: number) => Promise<SubExperimentDto>;
  fetchMeasurements: (subExperimentId: number) => Promise<MeasurementDto[]>;

  analyzeSubexperiment: (
    subExperimentId: number,
    sub: AnalyzeNewSubexperimentApi
  ) => Promise<ResultDto>;

  newSubexperiment: (
    subExperimentId: number,
    sub: AnalyzeNewSubexperimentApi
  ) => Promise<SubExperimentDto>;

  fetchExperimentMetadata: (experimentId: number) => Promise<ExperimentMetadataDto>;
  makePrimary: (subExperimentId: number) => Promise<SubExperimentDto>;
  renameSubexperiment: (subExperimentId: number, newName: string) => Promise<SubExperimentDto>;
  deleteSubexperiment: (subExperimentId: number) => Promise<any>;
  changeSuspiciousFlag: (
    subExperimentId: number,
    isSuspicious: boolean
  ) => Promise<SubExperimentDto>;
}

export default function EdnaApi(axios: AxiosInstance): EdnaApiInterface {
  return {
    fetchCompounds: async (params: SortParamsApi): Promise<CompoundDto[]> => {
      return axios
        .get("/compounds", {
          params: {
            // TODO expand other fields
            sortby: params.sortby,
          },
        })
        .then(proj => proj.data);
    },

    updateChemSoftLink: async (compoundId: number, newLink: string): Promise<any> => {
      return axios
        .put(`/compound/chemsoft/${compoundId}`, `"${newLink}"`, {
          headers: { "Content-Type": "application/json" },
        })
        .then(proj => proj.data);
    },

    updateMdeLink: async (compoundId: number, newLink: string): Promise<any> => {
      return axios
        .put(`/compound/mde/${compoundId}`, `"${newLink}"`, {
          headers: { "Content-Type": "application/json" },
        })
        .then(proj => proj.data);
    },

    fetchTargets: async (params: SortParamsApi): Promise<TargetDto[]> => {
      return axios
        .get("/targets", {
          params: {
            // TODO expand other fields
            sortby: params.sortby,
          },
        })
        .then(proj => proj.data);
    },

    parseExcelFile: async (
      excelFile: Blob,
      onUploadProgress: (percent: number) => void
    ): Promise<ParsedExcelDto[]> => {
      const formData = new FormData();
      formData.append("file", excelFile);

      return axios
        .post("/file/parse", formData, {
          headers: {
            "Content-Type": "multipart/form-data",
          },
          onUploadProgress(progressEvent) {
            const percentCompleted = Math.round((progressEvent.loaded * 100) / progressEvent.total);
            onUploadProgress(percentCompleted);
          },
        })
        .then((response: AxiosResponse<ParsedExcelDto[]>) => response.data);
    },

    uploadExperiments: async (form: UploadExperimentsArgsApi) => {
      const formData = new FormData();
      formData.append("file", form.file);
      formData.append("projectId", form.projectId.toString());
      formData.append("methodologyId", form.methodologyId.toString());
      if (isDefined(form.description)) {
        formData.append("description", form.description);
      }

      return axios
        .post("/file/upload", formData, {
          headers: {
            "Content-Type": "multipart/form-data",
          },
        })
        .then(x => x.data);
    },

    createProject: async (args: CreateProjectArgsApi) => {
      return axios
        .post("/project", args)
        .then(resp => resp.data)
        .catch(error => {
          throw new Error(error.response.data);
        });
    },

    editProject: async (projId: number, args: CreateProjectArgsApi) => {
      return axios
        .put(`/project/${projId}`, args)
        .then(resp => resp.data)
        .catch(error => {
          throw new Error(error.response.data);
        });
    },

    fetchProjects: async (params?: SortParamsApi) => {
      return axios
        .get("/projects", {
          params: {
            // TODO expand other fields
            sortby: params?.sortby,
          },
        })
        .then(proj => proj.data);
    },

    createMethodology: async (args: CreateMethodologyArgsApi) => {
      return axios
        .post("/methodology", args)
        .then(resp => resp.data)
        .catch(error => {
          throw new Error(error.response.data);
        });
    },

    editMethodology: async (methId: number, args: CreateMethodologyArgsApi) => {
      return axios
        .put(`/methodology/${methId}`, args)
        .then(resp => resp.data)
        .catch(error => {
          throw new Error(error.response.data);
        });
    },

    deleteMethodology: async (methId: number) => {
      return axios
        .delete(`/methodology/${methId}`)
        .then(resp => resp.data)
        .catch(error => {
          throw new Error(error.response.data);
        });
    },

    fetchMethodologies: async (params: SortParamsApi) => {
      return axios
        .get("/methodologies", {
          params: {
            // TODO expand other fields
            sortdy: params.sortby,
          },
        })
        .then(proj => proj.data);
    },

    fetchExperiments: async (
      projectId?: number,
      compoundId?: number,
      targetId?: number,
      params?: SortParamsApi
    ) => {
      return axios
        .get("/experiments", {
          params: {
            projectId,
            compoundId,
            targetId,
            // TODO expand other fields
            sortby: params?.sortby,
          },
        })
        .then(experiments => experiments.data);
    },

    fetchSubExperiment: async (subExperimentId: number) => {
      return axios.get(`/subExperiment/${subExperimentId}`).then(subExp => subExp.data);
    },

    fetchMeasurements: async (subExperimentId: number) => {
      return axios
        .get(`/subExperiment/${subExperimentId}/measurements`)
        .then(subExp => subExp.data);
    },

    analyzeSubexperiment: async (subExperimentId: number, sub: AnalyzeNewSubexperimentApi) => {
      return axios.post(`/subExperiment/${subExperimentId}/new/analyse`, sub).then(res => res.data);
    },

    newSubexperiment: async (subExperimentId: number, sub: AnalyzeNewSubexperimentApi) => {
      return axios.post(`/subExperiment/${subExperimentId}/new`, sub).then(res => res.data);
    },

    fetchExperimentMetadata: async (experimentId: number) => {
      return axios.get(`/experiment/${experimentId}/metadata`).then(res => res.data);
    },

    makePrimary: async (subExperimentId: number) => {
      return axios.post(`/subExperiment/primary/${subExperimentId}`).then(res => res.data);
    },

    renameSubexperiment: async (subExperimentId: number, newName: string) => {
      return axios
        .put(`/subExperiment/name/${subExperimentId}`, `"${newName}"`, {
          headers: { "Content-Type": "application/json" },
        })
        .then(res => res.data);
    },

    deleteSubexperiment: async (subExperimentId: number) => {
      return axios.delete(`/subExperiment/${subExperimentId}`).then(res => res.data);
    },

    changeSuspiciousFlag: async (subExperimentId: number, isSuspicious: boolean) => {
      return axios
        .put(`/subExperiment/suspicious/${subExperimentId}`, `${isSuspicious}`, {
          headers: { "Content-Type": "application/json" },
        })
        .then(res => res.data);
    },
  };
}
