import React from "react";
import { useRecoilValue } from "recoil";
import SuccCheckmark from "../../assets/svg/success-checkmark.svg";
import FailedIcon from "../../assets/svg/failed.svg";
import { excelFileAtom } from "../../store/atoms";
import "./UploadStatus.scss";

export function UploadStatus(): React.ReactElement {
  const uploadState = useRecoilValue(excelFileAtom);

  return uploadState ? (
    <div className="uploadStatus">
      <div className="uploadStatus__line">
        <span className="uploadStatus__label">
          {uploadState.state === "uploading"
            ? "Uploading..."
            : uploadState.state === "verifying"
            ? "Verifying"
            : uploadState.state === "parsed"
            ? "Parsed"
            : uploadState.state === "failed-to-parse"
            ? "Failed to parse"
            : uploadState.state === "failed-to-add"
            ? "Failed to add"
            : uploadState.state === "added"
            ? "Added"
            : ""}
        </span>
        {uploadState.state === "parsed" && <SuccCheckmark />}
        {uploadState.state === "failed-to-parse" && <FailedIcon />}
        {uploadState.state === "failed-to-add" && <FailedIcon />}
      </div>
      {uploadState.state === "uploading" && <ProgressBar percent={uploadState.progress} />}
    </div>
  ) : (
    <></>
  );
}

interface ProgressBarProps {
  percent: number;
}

function ProgressBar({ percent }: ProgressBarProps) {
  return (
    <div className="progressBar">
      <div className="progressBar__label">{percent}%</div>
      <div className="progressBar__bar">
        <div className="progressBar__progress" style={{ width: `${percent}%` }} />
      </div>
    </div>
  );
}
