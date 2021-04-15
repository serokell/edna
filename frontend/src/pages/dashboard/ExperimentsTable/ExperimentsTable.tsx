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
import { formatDateTimeDto, formatIC50, isDefined } from "../../../utils/utils";
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
        "Right" in e.primarySubExperiment.item.result ? (
          formatIC50(e.primarySubExperiment.item.result.Right[2])
        ) : (
          <span className="ic50__valueNone" />
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
                  addSubExperiment(exp.primarySubExperiment.id);
                } else {
                  // TODO fix it
                  if (selectedSubexperiments.size === 1) {
                    setShowEntries("all");
                  }

                  removeSubExperiments(exp.subExperiments);
                }
              }}
            />
            <span className="ednaCheckbox__checkmark" />
          </label>
        </td>
      ),
    }),
    [
      expTableSize,
      selectedSubexperiments,
      selectedExperiments,
      addSubExperiment,
      removeSubExperiments,
    ]
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
        accessor: (e: Experiment) => formatDateTimeDto(e.uploadDate),
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
                    description: metadata.fileMetadata.concat(metadata.description),
                  });
                }}
              />,
              <a
                key="download"
                download
                className="contextActions__item"
                href={`/experiment/${e.id}/file`}
                onMouseDown={() => {
                  console.log("on link click");
                }}
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
          subexperiment={s}
        />
      ))}
    </div>
  );
}
