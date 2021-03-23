// The module might be removed if we start using open api client generator
import { AxiosInstance, AxiosResponse } from "axios";
import { Experiment, Methodology } from "../store/types";
import { delay } from "../utils/utils";
import {
  CompoundDto,
  genRandomProject,
  genRandomTarget,
  genSeq,
  groupCompounds,
  MeasurementDto,
  ParsedExcelDto,
  ProjectDto,
  TargetDto,
} from "./types";

export interface UploadExperimentsArgs {
  file: File;
  projectId: number;
  methodologyId: number;
  description: string;
}

interface CreateMethodologyArgs {
  name: string;
  description: string;
  confluence: string;
}

interface CreateProjectArgs {
  name: string;
  description: string;
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

  uploadExperiments(form: UploadExperimentsArgs): Promise<unknown>;
  fetchProjects: () => Promise<ProjectDto[]>;
  createProject: (args: CreateProjectArgs) => Promise<ProjectDto>;
  fetchTargets: () => Promise<TargetDto[]>;
  fetchCompounds: () => Promise<CompoundDto[]>;
  fetchMethodologies: () => Promise<Methodology[]>;
  createMethodology: (args: CreateMethodologyArgs) => Promise<Methodology>;
}

export default function EdnaApi(axios: AxiosInstance): EdnaApiInterface {
  return {
    fetchCompounds: async (): Promise<CompoundDto[]> => {
      await delay(1000);
      return genSeq(10, genRandomTarget);
    },

    fetchTargets: async (): Promise<TargetDto[]> => {
      await delay(1000);
      return genSeq(10, genRandomTarget);
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
            target: "target",
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

    uploadExperiments: async (form: UploadExperimentsArgs) => {
      return axios.post("/addExperiments", form, {
        headers: {
          "Content-Type": "multipart/form-data",
        },
      });
    },

    createProject: async (args: CreateProjectArgs) => {
      // TODO send a request to backend, show some notifications on result
      // delay up to 3 seconds
      await delay(Math.random() * 2000);
      // eslint-disable-next-line @typescript-eslint/ban-types
      return axios
        .post("/project", args)
        .then(() => {
          return genRandomProject();
        })
        .catch(error => {
          throw new Error(error.response.data);
        });
    },

    fetchProjects: async () => {
      // TODO remove delay. For network emulation purposes
      // await delay(1000);
      const randomProjs: ProjectDto[] = genSeq(10, genRandomProject);
      return randomProjs.concat([
        {
          id: 1,
          item: {
            name: "Project 1",
            description: "Supa pupa project 1",
          },
          extra: {
            creationDate: 1000000000,
            lastUpdate: 1000000000,
            compoundNames: ["Nl", "H", "Li"],
          },
        },

        {
          id: 2,
          item: {
            name: "Project 2",
            description: "Supa pupa project 2",
          },
          extra: {
            creationDate: 2000000000,
            lastUpdate: 2000000000,
            compoundNames: ["Am", "Ke", "U", "F"],
          },
        },

        {
          id: 3,
          item: {
            name: "Project 3",
            description:
              "There is a long long description of the project kjdsjdssldjsghfj she jksgh",
          },
          extra: {
            creationDate: 3000000000,
            lastUpdate: 3000000000,
            compoundNames: ["Am", "Ke", "U", "F"],
          },
        },
      ]);
    },

    createMethodology: async (args: CreateMethodologyArgs) => {
      // TODO parse response from backend
      // delay up to 3 seconds
      await delay(Math.random() * 2000);
      // eslint-disable-next-line @typescript-eslint/ban-types
      return axios
        .post("/methodology", args)
        .then(() => {
          const methId = Math.floor(Math.random() * 1000000000);
          return { methodologyId: methId, name: args.name };
        })
        .catch(error => {
          throw new Error(error.response.data);
        });
    },

    fetchMethodologies: async () => {
      // TODO remove delay. For network emulation purposes
      await delay(2000);
      return [
        {
          methodologyId: 1,
          name: "Meth 1",
          description: "Cool cool methodology 1",
        },
        {
          methodologyId: 2,
          name: "Meth 2",
          description: "Cool cool methodology 2",
        },
      ];
    },
  };
}
