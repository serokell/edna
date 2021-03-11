import React, { FunctionComponent, ReactElement, useRef } from "react";
import "./UploadArea.scss";
import UploadSvg from "../../assets/svg/upload-svg.svg";
import { FormikCompatible } from "../FormField/FormField";

type UploadingAreaProps = FormikCompatible<File>;

const UploadArea: FunctionComponent<UploadingAreaProps> = ({ value, onChange }): ReactElement => {
  const uploadInputRef = useRef<HTMLInputElement | null>(null);

  return (
    <>
      <input
        ref={uploadInputRef}
        className="invisible"
        type="file"
        accept=".xlsx,.xls"
        onChange={e => {
          if (e.target.files && e.target.files.length > 0) {
            onChange(e.target.files[0]);
            // reset file path to fire onChange event even if the same file is
            // chosen again https://stackoverflow.com/a/54632736
            e.target.value = "";
          }
        }}
      />

      <div
        className="uploadArea"
        onClick={e => {
          e.stopPropagation();
          if (uploadInputRef.current) uploadInputRef.current?.click();
        }}
      >
        <div className="uploadArea__innerRect">
          <UploadSvg className="uploadArea__uploadSvg" />
          {value ? value.name : <strong>Click to upload</strong>}
        </div>
      </div>
    </>
  );
};

export default UploadArea;
