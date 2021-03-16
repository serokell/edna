import React, { FunctionComponent, ReactElement, useRef } from "react";
import "./UploadArea.scss";
import { useSetRecoilState } from "recoil";
import UploadSvg from "../../assets/svg/upload-svg.svg";
import { FormikCompatible } from "../FormField/FormField";
import Api from "../../api/api";
import { excelFileAtom } from "../../store/atoms";
import { delay } from "../../utils/utils";

type UploadingAreaProps = FormikCompatible<File>;

const UploadArea: FunctionComponent<UploadingAreaProps> = ({ value, onChange }): ReactElement => {
  const uploadInputRef = useRef<HTMLInputElement | null>(null);
  const setParsedFile = useSetRecoilState(excelFileAtom);

  return (
    <>
      <input
        ref={uploadInputRef}
        className="invisible"
        type="file"
        accept=".xlsx,.xls"
        onChange={async e => {
          if (e.target.files && e.target.files.length > 0) {
            const file = e.target.files[0];
            onChange(file);
            // reset file path to fire onChange event even if the same file is
            // chosen again https://stackoverflow.com/a/54632736
            e.target.value = "";

            setParsedFile({ state: "uploading", progress: 0 });
            try {
              // TODO remove it
              // await delay(1000);
              // setParsedFile({ state: "uploading", progress: 30 });
              // await delay(1000);

              const parsed = await Api.parseExcelFile(file, percent => {
                if (percent === 100) setParsedFile({ state: "verifying" });
                // TODO uncomment it
                // else setParsedFile({ state: "uploading", progress: percent });
              });
              setParsedFile({ state: "parsed", targets: parsed[0], experiments: parsed[1] });
            } catch (er) {
              setParsedFile({ state: "failed-to-parse", reason: er.message });
            }
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
