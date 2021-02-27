import React, { FunctionComponent, ReactElement, useRef } from "react";
import "./UploadArea.scss";
import UploadSvg from "../../assets/svg/upload-svg.svg";

interface UploadingAreaProps {
  chosenFile?: File;
  onFileChosen: (f: File) => void;
  [key: string]: any;
}

const UploadArea: FunctionComponent<UploadingAreaProps> = ({
  chosenFile,
  onFileChosen,
  ...props
}): ReactElement => {
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
            onFileChosen(e.target.files[0]);
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
          {chosenFile ? (
            chosenFile.name
          ) : (
            <>
              Drop your file here, or <strong>click to upload</strong>
            </>
          )}
        </div>
      </div>
    </>
  );
};

export default UploadArea;
