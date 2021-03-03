import React, { FunctionComponent, ReactElement, useRef } from "react";
import "./UploadArea.scss";
import { useField } from "formik";
import UploadSvg from "../../assets/svg/upload-svg.svg";

interface UploadingAreaProps {
  [key: string]: any;
  name: string;
}

const UploadArea: FunctionComponent<UploadingAreaProps> = ({ name, ...props }): ReactElement => {
  // eslint-disable-next-line no-empty-pattern
  const [{}, { value }, { setValue }] = useField<File>(name);
  const uploadInputRef = useRef<HTMLInputElement | null>(null);

  return (
    <>
      <input
        ref={uploadInputRef}
        style={{ display: "none" }}
        type="file"
        accept=".xlsx,.xls"
        onChange={e => {
          if (e.target.files && e.target.files.length > 0) {
            setValue(e.target.files[0]);
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
        <div {...props} className="uploadArea__innerRect">
          <UploadSvg className="uploadArea__uploadSvg" />
          {value ? value.name : <strong>Click to upload</strong>}
        </div>
      </div>
    </>
  );
};

export default UploadArea;
