// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import { useRecoilCallback, useRecoilValue, useSetRecoilState, waitForAll } from "recoil";
import React, { useEffect, useState } from "react";
import { Column } from "react-table";
import cx from "classnames";
import { v4 as uuidv4 } from "uuid";
import {
  experimentMetadata,
  experimentsTableSizeAtom,
  modalDialogAtom,
  selectedSubExperimentsIdsAtom,
  subExperimentsMetaAtom,
} from "../../../store/atoms";
import { Table } from "../../../components/Table/Table";
import { filteredExperimentsQuery, selectedExperimentsQuery } from "../../../store/selectors";
import { formatAsDate, formatIC50, isDefined } from "../../../utils/utils";
import { ContextActions } from "../../../components/ContextActions/ContextActions";
import { EmptyPlaceholder } from "../../../components/EmptyPlaceholder/EmptyPlaceholder";
import { Experiment } from "../../../store/types";
import "../../../components/Table/Table.scss";
import { Button } from "../../../components/Button/Button";
import "./ExperimentsTable.scss";
import { useAddSubExperiment, useRemoveSubExperiments } from "../../../store/updaters";
import { ExpandMinimizeButton } from "../ExpandMinimizeButton/ExpandMinimizeButton";
import { SubexperimentPlate } from "../SubexperimentPlate/SubexperimentPlate";
import { SuspenseSpinner } from "../../../components/Spinner/SuspsenseSpinner";
import cn from "../../../utils/bemUtils";
import "../IC50Line.scss";
import DownloadSvg from "../../../assets/svg/download.svg";
import { ContextItem } from "../../../components/ContextActions/ContextItems";
import { Tooltip } from "../../../components/Tooltip/Tooltip";

interface ExperimentsTableSuspendableProps {
  className?: string;
}

export function ExperimentsTableSuspendable({
  className,
}: ExperimentsTableSuspendableProps): React.ReactElement {
  const { experiments } = useRecoilValue(filteredExperimentsQuery);
  const [showEntries, setShowEntries] = useState<ShowEntries>("all");
  const selectedSubexperiments = useRecoilValue(selectedSubExperimentsIdsAtom);
  const addSubExperiment = useAddSubExperiment();
  const removeSubExperiments = useRemoveSubExperiments();
  const selectedExperiments = useRecoilValue(selectedExperimentsQuery);
  const expTableSize = useRecoilValue(experimentsTableSizeAtom);
  const setModalDialog = useSetRecoilState(modalDialogAtom);

  useEffect(() => {
    const subsToRemove = Array.from(selectedSubexperiments).filter(
      subId => !isDefined(experiments.find(e => e.subExperiments.find(s => s === subId)))
    );
    if (subsToRemove.length !== 0) {
      removeSubExperiments(subsToRemove);
    }
  }, [selectedSubexperiments, removeSubExperiments, experiments]);

  useEffect(() => {
    if (selectedExperiments.size === 0) {
      setShowEntries("all");
    }
  }, [selectedExperiments.size]);

  const cacheMetadata = useRecoilCallback(
    ({ snapshot }) => (experimentId: number) => {
      return snapshot.getPromise(experimentMetadata(experimentId));
    },
    [experimentMetadata]
  );

  const compoundColumn = React.useMemo(
    () => ({
      Header: "Compound",
      accessor: (e: Experiment) => e.compoundName,
    }),
    []
  );

  const targetColumn = React.useMemo(
    () => ({
      Header: "Target",
      accessor: (e: Experiment) => e.targetName,
    }),
    []
  );

  const ic50Column = React.useMemo(
    () => ({
      Header: "IC50",
      accessor: (e: Experiment) =>
        "Right" in e.primaryIC50 ? (
          <Tooltip text={`${e.primaryIC50.Right}`}>{formatIC50(e.primaryIC50.Right)}</Tooltip>
        ) : (
          <Tooltip text={e.primaryIC50.Left} type="error">
            <span className="ic50__valueNone" />
          </Tooltip>
        ),
    }),
    []
  );

  const showCheckboxColumn = React.useMemo(
    () => ({
      id: "show",
      accessor: (exp: Experiment) => (
        <td
          className={`ednaTable__cell ${
            expTableSize === "minimized" ? "experimentsTable__showCheckbox" : ""
          }`}
        >
          <label className="ednaCheckbox" htmlFor={exp.id.toString()}>
            <input
              id={exp.id.toString()}
              type="checkbox"
              checked={selectedExperiments.has(exp.id)}
              onChange={e => {
                if (e.target.checked) {
                  addSubExperiment(exp.primarySubExperimentId);
                } else {
                  removeSubExperiments(exp.subExperiments);
                }
              }}
            />
            <span className="ednaCheckbox__checkmark" />
          </label>
        </td>
      ),
    }),
    [expTableSize, selectedExperiments, addSubExperiment, removeSubExperiments]
  );

  const minimizedColumns: Column<Experiment>[] = React.useMemo(
    () => [compoundColumn, targetColumn, ic50Column, showCheckboxColumn],
    [compoundColumn, targetColumn, ic50Column, showCheckboxColumn]
  );

  // TODO implement file downloading
  const expandedColumns: Column<Experiment>[] = React.useMemo(
    () => [
      compoundColumn,
      targetColumn,
      ic50Column,
      {
        Header: "Methodology",
        accessor: (e: Experiment) => e.methodologyName,
      },
      {
        Header: "Upload data",
        accessor: (e: Experiment) => formatAsDate(e.uploadDate),
      },
      showCheckboxColumn,
      {
        id: "actions",
        accessor: (e: Experiment) => (
          <ContextActions
            actions={[
              <ContextItem
                key="metadata"
                type="metadata"
                onClick={async () => {
                  const metadata = await cacheMetadata(e.id);
                  setModalDialog({
                    kind: "show-experiment-metadata",
                    description: [metadata.description].concat(metadata.fileMetadata),
                  });
                }}
              />,
              <a
                key="download"
                download
                className="contextActions__item"
                href={`/api/experiment/${e.id}/file`}
              >
                <DownloadSvg />
                Download
              </a>,
            ]}
          />
        ),
      },
    ],
    [setModalDialog, cacheMetadata, compoundColumn, targetColumn, ic50Column, showCheckboxColumn]
  );

  return (
    <div className={cx("experimentsArea", className)}>
      {experiments.length === 0 ? (
        <EmptyPlaceholder title="No experiments match" />
      ) : (
        <>
          <div className="experimentsArea__controlBtns">
            <ShowEntriesSwitch
              value={showEntries}
              onChange={setShowEntries}
              disabledSelected={selectedExperiments.size === 0}
            />
            <ExpandMinimizeButton targetSize="expanded" />
          </div>
          <div className="tableContainer experimentsArea__experimentsTableContainer">
            <Table<Experiment>
              small
              columns={expTableSize === "minimized" ? minimizedColumns : expandedColumns}
              data={
                showEntries === "all"
                  ? experiments
                  : experiments.filter(e => selectedExperiments.has(e.id))
              }
              columnExtras={{
                show: { manualCellRendering: true },
              }}
              collapsible={e => (
                <SuspenseSpinner>
                  <ExperimentsCollapse experiment={e} expanded={expTableSize === "expanded"} />
                </SuspenseSpinner>
              )}
            />
          </div>
        </>
      )}
    </div>
  );
}

type ShowEntries = "all" | "selected";

interface ShowEntriesSwitchProps {
  value: ShowEntries;
  onChange: (newVal: ShowEntries) => void;
  disabledSelected?: boolean;
}

function ShowEntriesSwitch({
  value,
  onChange,
  disabledSelected,
}: ShowEntriesSwitchProps): React.ReactElement {
  return (
    <div className="experimentsArea__showEntries">
      <Button
        active={value === "all"}
        className="experimentsArea__showBtn"
        type="half-rounded"
        size="small"
        onClick={() => onChange("all")}
      >
        Show all
      </Button>

      <Button
        active={value === "selected"}
        disabled={disabledSelected}
        className="experimentsArea__showBtn"
        type="half-rounded"
        size="small"
        onClick={() => onChange("selected")}
      >
        Show selected
      </Button>
    </div>
  );
}

function ExperimentsCollapse({
  experiment,
  expanded,
}: {
  experiment: Experiment;
  expanded: boolean;
}) {
  const subExperiments = useRecoilValue(
    waitForAll(experiment.subExperiments.map(subId => subExperimentsMetaAtom(subId)))
  );

  const experimentsTable = cn("experimentsTable");

  return (
    <div className={experimentsTable("subexperiments")}>
      {subExperiments.map(s => (
        <SubexperimentPlate
          className={experimentsTable("subexperiment", { expanded })}
          key={uuidv4()}
          isPrimary={experiment.primarySubExperimentId === s.id}
          subexperiment={s}
        />
      ))}
    </div>
  );
}
