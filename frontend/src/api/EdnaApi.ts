// The module might be removed if we start using open api client generator

import { AxiosInstance, AxiosResponse } from "axios";
import { ExperimentalMeasurementDto } from "./types";

interface EdnaApiInterface {
  uploadExperiment: (excelFile: Blob) => Promise<ExperimentalMeasurementDto[]>
}

export default function EdnaApi(axios: AxiosInstance): EdnaApiInterface {
  return {
    uploadExperiment: async (excelFile: Blob): Promise<ExperimentalMeasurementDto[]> => {
      const formData = new FormData();
      formData.append("file", excelFile);
      return axios
        .post("/experiment", formData, {
          headers: {
            "Content-Type": "multipart/form-data",
          },
        })
        .then((response: AxiosResponse<ExperimentalMeasurementDto[]>) => response.data);
    },
  };
}
