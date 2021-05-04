// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
  selectorFamily,
  useRecoilCallback,
  useRecoilState,
  useRecoilValue,
  useSetRecoilState,
  waitForAll,
} from "recoil";
import React, { useCallback, useEffect, useState } from "react";
import { Column } from "react-table";
import cx from "classnames";
import { v4 as uuidv4 } from "uuid";
import {
  compoundIdSelectedAtom,
  experimentMetadata,
  experimentsTableSizeAtom,
  modalDialogAtom,
  projectSelectedIdAtom,
  selectedSubExperimentsIdsAtom,
  targetIdSelectedAtom,
  subExperimentsMetaMap,
} from "../../../store/atoms";
import { Table } from "../../../components/Table/Table";
import {
  defaultBatchQuery,
  filteredExperimentsDtoQuery,
  filteredExperimentsQuery,
} from "../../../store/selectors";
import { formatAsDate, formatAsDateTime, isDefined } from "../../../utils/utils";
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
import DownloadSvg from "../../../assets/svg/download.svg";
import { ContextItem } from "../../../components/ContextActions/ContextItems";
import { Tooltip } from "../../../components/Tooltip/Tooltip";
import { SortParamsApi } from "../../../api/EdnaApi";
import { IC50Tooltip } from "../../../components/IC50Line/IC50Tooltip";

interface ExperimentsTableSuspendableProps {
  className?: string;
}

export function ExperimentsTableSuspendable({
  className,
}: ExperimentsTableSuspendableProps): React.ReactElement {
  const expTableSize = useRecoilValue(experimentsTableSizeAtom);
  const setModalDialog = useSetRecoilState(modalDialogAtom);
  const [selectedSubexperiments, setSelectedSubexperiments] = useRecoilState(
    selectedSubExperimentsIdsAtom
  );
  const selectedProjectId = useRecoilValue(projectSelectedIdAtom);
  const selectedCompoundId = useRecoilValue(compoundIdSelectedAtom);
  const selectedTargetId = useRecoilValue(targetIdSelectedAtom);

  // TODO request here only 1st page to check experiments emptiness
  const experimentsChunk = useRecoilValue(
    defaultBatchQuery(filteredExperimentsDtoQuery, { sortby: "uploadDate", desc: true })
  ).experiments;
  const [showEntries, setShowEntries] = useState<ShowEntries>("all");
  const addSubExperiment = useAddSubExperiment();
  const removeSubExperiments = useRemoveSubExperiments();

  const ifExperimentSelected = useCallback(
    (exp: Experiment) => {
      return isDefined(exp.subExperiments.find(x => selectedSubexperiments.has(x)));
    },
    [selectedSubexperiments]
  );

  const shownExperiments = selectorFamily({
    key: "ShownExperiments",
    get: (args: [ShowEntries, SortParamsApi]) => ({ get }) => {
      const { experiments } = get(filteredExperimentsQuery(args[1]));
      return args[0] === "all" ? experiments : experiments.filter(ifExperimentSelected);
    },
  });

  // Remove sub-experiments on selector changes
  useEffect(() => {
    setSelectedSubexperiments(new Set());
  }, [selectedProjectId, selectedCompoundId, selectedTargetId, setSelectedSubexperiments]);

  useEffect(() => {
    if (selectedSubexperiments.size === 0) {
      setShowEntries("all");
    }
  }, [selectedSubexperiments.size]);

  const cacheMetadata = useRecoilCallback(
    ({ snapshot }) => (experimentId: number) => {
      return snapshot.getPromise(experimentMetadata(experimentId));
    },
    [experimentMetadata]
  );

  const compoundColumn = React.useMemo(
    () => ({
      Header: "Compound",
      disableSortBy: true,
      accessor: (e: Experiment) => e.compoundName,
    }),
    []
  );

  const targetColumn = React.useMemo(
    () => ({
      Header: "Target",
      disableSortBy: true,
      accessor: (e: Experiment) => e.targetName,
    }),
    []
  );

  const ic50Column = React.useMemo(
    () => ({
      Header: "IC50",
      disableSortBy: true,
      accessor: (e: Experiment) => <IC50Tooltip ic50={e.primaryIC50} />,
    }),
    []
  );

  const showCheckboxColumn = React.useMemo(
    () => ({
      id: "show",
      disableSortBy: true,
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
              checked={ifExperimentSelected(exp)}
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
    [expTableSize, ifExperimentSelected, addSubExperiment, removeSubExperiments]
  );

  const minimizedColumns: Column<Experiment>[] = React.useMemo(
    () => [compoundColumn, targetColumn, ic50Column, showCheckboxColumn],
    [compoundColumn, targetColumn, ic50Column, showCheckboxColumn]
  );

  const expandedColumns: Column<Experiment>[] = React.useMemo(
    () => [
      compoundColumn,
      targetColumn,
      ic50Column,
      {
        Header: "Methodology",
        disableSortBy: true,
        accessor: (e: Experiment) => e.methodologyName,
      },
      {
        Header: "Upload date",
        id: "uploadDate",
        accessor: (e: Experiment) => (
          <Tooltip text={formatAsDateTime(e.uploadDate)}>{formatAsDate(e.uploadDate)}</Tooltip>
        ),
      },
      showCheckboxColumn,
      {
        id: "actions",
        disableSortBy: true,
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
      {experimentsChunk.length === 0 ? (
        <EmptyPlaceholder title="No experiments match" />
      ) : (
        <>
          <div className="experimentsArea__controlBtns">
            <ShowEntriesSwitch
              value={showEntries}
              onChange={setShowEntries}
              disabledSelected={selectedSubexperiments.size === 0}
            />
            <ExpandMinimizeButton targetSize="expanded" />
          </div>
          <div className="tableContainer experimentsArea__experimentsTableContainer">
            <Table<Experiment>
              defaultSortedColumn="uploadDate"
              small
              columns={expTableSize === "minimized" ? minimizedColumns : expandedColumns}
              dataOrQuery={params => shownExperiments([showEntries, params])}
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

export type ShowEntries = "all" | "selected";

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
    waitForAll(experiment.subExperiments.map(subId => subExperimentsMetaMap(subId)))
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
