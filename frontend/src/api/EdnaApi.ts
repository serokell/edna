// The module might be removed if we start using open api client generator

import { AxiosInstance, AxiosResponse } from "axios";
import { MeasurementDto } from "./types";

interface EdnaApiInterface {
  uploadExperiment: (excelFile: Blob) => Promise<MeasurementDto[]>;
}

export default function EdnaApi(axios: AxiosInstance): EdnaApiInterface {
  return {
    uploadExperiment: async (excelFile: Blob): Promise<MeasurementDto[]> => {
      const formData = new FormData();
      formData.append("file", excelFile);
      return axios
        .post("/experiment", formData, {
          headers: {
            "Content-Type": "multipart/form-data",
          },
        })
        .then((response: AxiosResponse<MeasurementDto[]>) => response.data);
    },
  };
}
