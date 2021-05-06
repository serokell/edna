// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, ReactElement } from "react";
import { useRecoilValue } from "recoil";
import { useNotificationListUpdater } from "../../../store/updaters";
import { notificationListAtom } from "../../../store/atoms";
import cn from "../../../utils/bemUtils";
import "./NotificationList.scss";
import { Notification } from "../Notification/Notification";

const NotificationList: FunctionComponent = (): ReactElement => {
  const notificationList = cn("notificationList");
  const notifications = useRecoilValue(notificationListAtom);
  const notificationsUpdater = useNotificationListUpdater();
  return (
    <div
      className={notificationList()}
      onClick={() =>
        notificationsUpdater({
          type: "Add",
          notificationType: "Success",
          element: <span>{`NEW ${Math.random()}`}</span>,
        })
      }
    >
      {notifications.notifications.map(n => (
        <Notification key={n.id} id={n.id} type="Success">
          {n.element}
        </Notification>
      ))}
    </div>
  );
};

export default NotificationList;
