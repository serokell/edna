import { useRecoilValue, waitForAll } from "recoil";
import React, { useState } from "react";
import { Column } from "react-table";
import cx from "classnames";
import { v4 as uuidv4 } from "uuid";
import {
  experimentsTableSizeAtom,
  selectedSubExperimentsIdsAtom,
  subExperimentsMetaAtom,
} from "../../../store/atoms";
import { Table } from "../../../components/Table/Table";
import { filteredExperimentsQuery, selectedExperimentsQuery } from "../../../store/selectors";
import { formatDateTimeDto } from "../../../utils/utils";
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
      accessor: (e: Experiment) => e.primarySubExperiment.item.result[2],
    }),
    []
  );

  const showCheckboxColumn = React.useMemo(
    () => ({
      id: "show",
      accessor: (exp: Experiment) => (
        <input
          type="checkbox"
          checked={selectedExperiments.has(exp.id)}
          onChange={e => {
            if (e.target.checked) {
              addSubExperiment(exp.primarySubExperiment.id);
            } else {
              if (selectedSubexperiments.size === 1) {
                setShowEntries("all");
              }

              removeSubExperiments(exp.subExperiments);
            }
          }}
        />
      ),
    }),
    [selectedSubexperiments, selectedExperiments, addSubExperiment, removeSubExperiments]
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
        accessor: (e: Experiment) => e.methodologyName,
      },
      {
        Header: "Upload data",
        accessor: (e: Experiment) => formatDateTimeDto(e.uploadDate),
      },
      showCheckboxColumn,
      {
        id: "actions",
        accessor: () => <ContextActions actions={[]} />,
      },
    ],
    [compoundColumn, targetColumn, ic50Column, showCheckboxColumn]
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
              collapsible={e => (
                <SuspenseSpinner>
                  <ExperimentsCollapse experiment={e} />
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

function ExperimentsCollapse({ experiment }: { experiment: Experiment }) {
  const subExperiments = useRecoilValue(
    waitForAll(experiment.subExperiments.map(subId => subExperimentsMetaAtom(subId)))
  );

  return (
    <div className="experimentsTable__subexperiments">
      {subExperiments.map(s => (
        <SubexperimentPlate key={uuidv4()} subexperiment={s} />
      ))}
    </div>
  );
}
