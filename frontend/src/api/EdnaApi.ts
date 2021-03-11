// The module might be removed if we start using open api client generator
import { AxiosInstance, AxiosResponse } from "axios";
import { Experiment, Methodology, Project } from "../store/types";
import { delay } from "../utils/utils";
import { groupCompounds, MeasurementDto, ParsedTargetDto } from "./types";

export interface UploadExperimentsArgs {
  file: File;
  projectId: number;
  methodologyId: number;
  description: string;
}

interface EdnaApiInterface {
  parseExcelFile: (
    excelFile: Blob,
    onUploadProgress: (percent: number) => void
  ) => Promise<[ParsedTargetDto[], Experiment[]]>;

  uploadExperiments(form: UploadExperimentsArgs): Promise<unknown>;
  fetchProjects: () => Promise<Project[]>;
  createProject: (projectName: string) => Promise<Project>;
  fetchMethodologies: () => Promise<Methodology[]>;
  createMethodology: (methodologyName: string) => Promise<Methodology>;
}

export default function EdnaApi(axios: AxiosInstance): EdnaApiInterface {
  return {
    parseExcelFile: async (
      excelFile: Blob,
      onUploadProgress: (percent: number) => void
    ): Promise<[ParsedTargetDto[], Experiment[]]> => {
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
          return [
            [
              {
                target: "hujarget",
                isNew: true,
                compounds: Object.entries(byCompound).map(x => x[0]),
              },
            ],

            Object.entries(byCompound).map(([cmpId, measurements]) => ({
              target: "hujarget",
              compoundId: cmpId,
              measurements,
            })),
          ];
        });
    },

    uploadExperiments: async (form: UploadExperimentsArgs) => {
      return axios.post("/addExperiments", form, {
        headers: {
          "Content-Type": "multipart/form-data",
        },
      });
    },

    createProject: async (projectName: string) => {
      // TODO send a request to backend, show some notifications on result
      // delay up to 3 seconds
      await delay(Math.random() * 2000 + 1000);
      const projId = Math.floor(Math.random() * 1000000000);
      return { projectId: projId, name: projectName };
    },

    fetchProjects: async () => {
      // TODO remove delay. For network emulation purposes
      await delay(1000);
      return [
        {
          projectId: 1,
          name: "Project 1",
          description: "Supa pupa project 1",
        },
        {
          projectId: 2,
          name: "Project 2",
        },
        {
          projectId: 3,
          name: "Project 3",
        },
      ];
    },

    createMethodology: async (methodologyName: string) => {
      // TODO send a request to backend, show some notifications on result
      // delay up to 3 seconds
      await delay(Math.random() * 2000 + 1000);
      const methId = Math.floor(Math.random() * 1000000000);
      return { methodologyId: methId, name: methodologyName };
    },

    fetchMethodologies: async () => {
      // TODO remove delay. For network emulation purposes
      await delay(2000);
      return [
        {
          methodologyId: 1,
          name: "Meth 1",
        },
        {
          methodologyId: 2,
          name: "Meth 2",
        },
      ];
    },
  };
}
