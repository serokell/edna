// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import { useRecoilState } from "recoil";
import { Tooltip } from "../Tooltip/Tooltip";
import SuccCheckmark from "../../assets/svg/success-notify.svg";
import FailedIcon from "../../assets/svg/failed.svg";
import ResetCheckmark from "../../assets/svg/delete-stroke.svg";
import { excelFileAtom } from "../../store/atoms";
import "./UploadStatus.scss";
import { Button } from "../Button/Button";
import { isAdded } from "../../store/types";
import { useNotificationListUpdater } from "../../store/updaters";

export function UploadStatus(): React.ReactElement {
  const [uploadState, setExcelFile] = useRecoilState(excelFileAtom);
  const notificationsUpdater = useNotificationListUpdater();

  return uploadState ? (
    <div className="uploadStatus">
      <div className="uploadStatus__line">
        <span className="uploadStatus__label">
          {uploadState.state === "uploading"
            ? "Uploading..."
            : uploadState.state === "parsed"
            ? "Parsed"
            : uploadState.state === "failed-to-add"
            ? "Failed to add"
            : uploadState.state === "added"
            ? "Added"
            : ""}
        </span>
        {(uploadState.state === "parsed" || uploadState.state === "added") && (
          <div className="uploadStatus__statusIcons">
            <div className="uploadStatus__statusIcon">
              <SuccCheckmark />
            </div>
            <div
              className="uploadStatus__resetIcon"
              onClick={() => {
                setExcelFile(undefined);
                if (!isAdded(uploadState)) {
                  notificationsUpdater({
                    type: "Add",
                    notificationType: "Error",
                    element: manualRemove => (
                      <div className="uploadingForm__resetNotify">
                        <span>You have reset the file</span>
                        <Button
                          type="text"
                          className="uploadingForm__resetNotifyBtn"
                          onClick={() => {
                            setExcelFile(uploadState);
                            manualRemove();
                          }}
                        >
                          Undo
                        </Button>
                      </div>
                    ),
                  });
                }
              }}
            >
              <Tooltip text="Clear file field">
                <ResetCheckmark />
              </Tooltip>
            </div>
          </div>
        )}
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
