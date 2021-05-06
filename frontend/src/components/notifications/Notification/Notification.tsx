// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, ReactElement, useEffect } from "react";
import { useNotificationListUpdater } from "../../../store/updaters";
import "./Notification.scss";
import cn from "../../../utils/bemUtils";
import ErrorIcon from "../../../assets/svg/error-notify.svg";
import SuccessIcon from "../../../assets/svg/success-notify.svg";

export interface NotificationProps {
  id: number;
  type: "Success" | "Error";
}

export const Notification: FunctionComponent<NotificationProps> = ({
  id,
  children,
  type,
}): ReactElement => {
  const notification = cn("notification");

  const notificationsUpdater = useNotificationListUpdater();

  useEffect(() => {
    const interval = setInterval(() => {
      notificationsUpdater({ type: "Delete", id });
    }, 5000);

    return () => {
      clearInterval(interval);
    };
  }, [id, notificationsUpdater]);

  return (
    <div className={notification()}>
      {type === "Error" ? <ErrorIcon /> : <SuccessIcon />}
      <div>{children}</div>
    </div>
  );
};
