// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React, { FunctionComponent, useEffect, useState } from "react";
import cx from "classnames";
import "./LibraryPage.scss";
import { Route, Switch, Link, useLocation } from "react-router-dom";
import { Redirect } from "react-router";
import { SuspenseSpinner } from "../../components/Spinner/SuspsenseSpinner";
import PageLayout from "../../components/PageLayout/PageLayout";
import { CreateMethodologyButton } from "../../components/buttons/CreateMethodologyButton";
import { CreateProjectButton } from "../../components/buttons/CreateProjectButton";
import { ErrorPlaceholder } from "../../components/Error/ErrorPlaceholder";
import { ProjectsSuspendable } from "./Projects";
import { CompoundsSuspendable } from "./Compounds";
import { MethodsSuspendable } from "./Methodologies";
import { TargetsSuspendable } from "./Targets";
import { NotFound } from "../../components/NotFound/NotFound";
import { Maybe } from "../../utils/utils";

export const LibraryPage: FunctionComponent = () => {
  return (
    <PageLayout>
      <EntitiesTab
        renderAddButton={activeTab => {
          if (activeTab === "projects")
            return <CreateProjectButton className="libraryPage__addBtn" />;
          if (activeTab === "methodologies")
            return <CreateMethodologyButton className="libraryPage__addBtn" />;
          return <></>;
        }}
        render={entity => {
          // TODO work out error placeholder better
          return (
            <ErrorPlaceholder>
              <SuspenseSpinner>{suspendables[entity]()}</SuspenseSpinner>
            </ErrorPlaceholder>
          );
        }}
      />
    </PageLayout>
  );
};

const entities = {
  projects: "Projects",
  compounds: "Compounds",
  targets: "Targets",
  methodologies: "Methodologies",
};

const entitiesFlds: EntityType[] = Object.keys(entities) as EntityType[];

const suspendables = {
  projects: () => <ProjectsSuspendable />,
  compounds: () => <CompoundsSuspendable />,
  targets: () => <TargetsSuspendable />,
  methodologies: () => <MethodsSuspendable />,
};

type EntityType = keyof typeof entities;

interface EntitiesTabProps {
  render: (entity: EntityType) => React.ReactNode;
  renderAddButton: (activeTab: EntityType) => React.ReactNode;
}

function EntitiesTab({ render, renderAddButton }: EntitiesTabProps) {
  const { pathname } = useLocation();
  const [entityTab, setEntityTab] = useState<Maybe<EntityType>>(undefined);

  useEffect(() => {
    setEntityTab(entitiesFlds.find(fld => pathname.endsWith(fld)));
  }, [pathname]);

  const renderRoute = (entity: EntityType) => (
    <Route key={entity} path={`/library/${entity}`}>
      {render(entity)}
    </Route>
  );

  return (
    <>
      <div className="entityTabs">
        {Object.entries(entities).map(([tpStr, title]) => (
          <Link
            key={tpStr}
            to={`/library/${tpStr}`}
            className={cx("entityTabs__tab", entityTab === tpStr && "entityTabs__tab_active")}
            onClick={() => setEntityTab(tpStr as EntityType)}
          >
            {title}
          </Link>
        ))}

        {entityTab && renderAddButton(entityTab)}
      </div>

      <Switch>
        {entitiesFlds.map(entity => renderRoute(entity))}

        <Redirect exact from="/library" to="/library/projects" />

        <Route path="*">
          <NotFound />
        </Route>
      </Switch>
    </>
  );
}
