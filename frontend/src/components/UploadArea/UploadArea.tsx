// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, ReactElement, useRef } from "react";
import "./UploadArea.scss";
import { useSetRecoilState } from "recoil";
import UploadSvg from "../../assets/svg/upload-svg.svg";
import { FormikCompatible } from "../FormField/FormField";
import Api from "../../api/api";
import { excelFileAtom } from "../../store/atoms";

type UploadingAreaProps = FormikCompatible<File>;

const UploadArea: FunctionComponent<UploadingAreaProps> = ({ value, onChange }): ReactElement => {
  const uploadInputRef = useRef<HTMLInputElement | null>(null);
  const setParsedFile = useSetRecoilState(excelFileAtom);

  const handleFile = async (file: File) => {
    onChange(file);
    setParsedFile({ state: "uploading", progress: 0 });
    try {
      const targets = await Api.parseExcelFile(file, p =>
        setParsedFile({ state: "uploading", progress: p })
      );
      setParsedFile({
        state: "parsed",
        targets,
      });
    } catch (er) {
      setParsedFile({ state: "failed-to-parse", reason: er.message });
    }
  };

  return (
    <>
      <input
        ref={uploadInputRef}
        className="invisible"
        type="file"
        accept=".xlsx,.xls"
        onChange={async e => {
          if (e.target.files && e.target.files.length === 1) {
            const file = e.target.files[0];
            // reset file path to fire onChange event even if the same file is
            // chosen again https://stackoverflow.com/a/54632736
            e.target.value = "";
            await handleFile(file);
          }
        }}
      />

      <div
        className="uploadArea"
        onClick={e => {
          e.stopPropagation();
          if (uploadInputRef.current) {
            uploadInputRef.current?.click();
          }
        }}
        onDragOver={async e => {
          e.preventDefault();
        }}
        onDrop={async e => {
          e.preventDefault();
          e.stopPropagation();
          if (e.dataTransfer.files && e.dataTransfer.files.length === 1) {
            await handleFile(e.dataTransfer.files[0]);
            e.dataTransfer.clearData();
          }
        }}
      >
        <UploadSvg className="uploadArea__uploadSvg" />
        {value ? (
          value.name
        ) : (
          <span className="uploadArea__description">Drop your file here, or click to upload</span>
        )}
      </div>
    </>
  );
};

export default UploadArea;
