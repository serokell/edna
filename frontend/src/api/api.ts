// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

// The module might be removed if we start using open api client generator

import axios, { AxiosInstance, AxiosRequestConfig } from "axios";
import EdnaApi from "./EdnaApi";

const axiosConfig: AxiosRequestConfig = {
  baseURL: "/api",
};

const axiosInstance: AxiosInstance = axios.create(axiosConfig);

const Api = EdnaApi(axiosInstance);

export default Api;
