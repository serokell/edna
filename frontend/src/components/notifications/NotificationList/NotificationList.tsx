// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, ReactElement } from "react";
import { useRecoilValue } from "recoil";
import { notificationListAtom } from "../../../store/atoms";
import cn from "../../../utils/bemUtils";
import "./NotificationList.scss";
import { Notification } from "../Notification/Notification";
import { useNotificationListUpdater } from "../../../store/updaters";

const NotificationList: FunctionComponent = (): ReactElement => {
  const notificationList = cn("notificationList");
  const notifications = useRecoilValue(notificationListAtom);
  const notificationsUpdater = useNotificationListUpdater();
  return (
    <div className={notificationList()}>
      {notifications.notifications.map(n => (
        <Notification key={n.id} id={n.id} type={n.type}>
          {n.element(() => notificationsUpdater({ type: "Delete", id: n.id }))}
        </Notification>
      ))}
    </div>
  );
};

export default NotificationList;
