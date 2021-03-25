// The module might be removed if we start using open api client generator
import { AxiosInstance, AxiosResponse } from "axios";
import { Experiment } from "../store/types";
import {
  CompoundDto,
  groupCompounds,
  MeasurementDto,
  MethodologyDto,
  ParsedExcelDto,
  ProjectDto,
  TargetDto,
} from "./types";
import { Maybe } from "../utils/utils";

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

export interface CreateProjectArgsApi {
  name: string;
  description: Maybe<string>;
}

interface EdnaApiInterface {
  // TODO will be removed in future
  oldParseExcelFile: (
    excelFile: Blob,
    onUploadProgress: (percent: number) => void
  ) => Promise<Experiment[]>;

  parseExcelFile: (
    excelFile: Blob,
    onUploadProgress: (percent: number) => void
  ) => Promise<ParsedExcelDto[]>;

  uploadExperiments(form: UploadExperimentsArgsApi): Promise<unknown>;
  fetchProjects: () => Promise<ProjectDto[]>;
  createProject: (args: CreateProjectArgsApi) => Promise<ProjectDto>;
  fetchTargets: () => Promise<TargetDto[]>;
  fetchCompounds: () => Promise<CompoundDto[]>;
  fetchMethodologies: () => Promise<MethodologyDto[]>;
  createMethodology: (args: CreateMethodologyArgsApi) => Promise<MethodologyDto>;
}

export default function EdnaApi(axios: AxiosInstance): EdnaApiInterface {
  return {
    fetchCompounds: async (): Promise<CompoundDto[]> => {
      return axios.get("/compounds").then(proj => proj.data);
    },

    fetchTargets: async (): Promise<TargetDto[]> => {
      return axios.get("/targets").then(proj => proj.data);
    },

    oldParseExcelFile: async (
      excelFile: Blob,
      onUploadProgress: (percent: number) => void
    ): Promise<Experiment[]> => {
      const formData = new FormData();
      formData.append("file", excelFile);
      return axios
        .post("/experiment", formData, {
          headers: {
            "Content-Type": "multipart/form-data",
          },
          onUploadProgress(progressEvent) {
            const percentCompleted = Math.round((progressEvent.loaded * 100) / progressEvent.total);
            onUploadProgress(percentCompleted);
          },
        })
        .then((response: AxiosResponse<MeasurementDto[]>) => {
          // TODO remove this mock transformation
          const byCompound = groupCompounds(response.data);
          return Object.entries(byCompound).map(([cmpId, measurements]) => ({
            target: "",
            compoundId: cmpId,
            measurements,
          }));
        });
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

      return axios.post(
        `/file/upload/${form.projectId}/${form.methodologyId}${
          form.description ? `?description=${form.description}` : ""
        }`,
        formData,
        {
          headers: {
            "Content-Type": "multipart/form-data",
          },
        }
      );
    },

    createProject: async (args: CreateProjectArgsApi) => {
      return axios
        .post("/project", args)
        .then(resp => resp.data)
        .catch(error => {
          throw new Error(error.response.data);
        });
    },

    fetchProjects: async () => {
      return axios.get("/projects").then(proj => proj.data);
    },

    createMethodology: async (args: CreateMethodologyArgsApi) => {
      return axios
        .post("/methodology", args)
        .then(resp => resp.data)
        .catch(error => {
          throw new Error(error.response.data);
        });
    },

    fetchMethodologies: async () => {
      return axios.get("/methodologies").then(proj => proj.data);
    },
  };
}
