import React from "react";
import { v4 as uuidv4 } from "uuid";
import { ContextActions } from "../ContextActions/ContextActions";
import "./DescriptivePlate.scss";
import { isDefined } from "../../utils/utils";

export interface EntityProperty {
  label: string;
  value: React.ReactNode;
}

interface DescriptivePlateProps {
  properties: EntityProperty[];
  contextActions?: React.ReactNode[];
}

export function DescriptivePlate({
  properties,
  contextActions,
}: DescriptivePlateProps): React.ReactElement {
  return (
    <div className="descriptivePlate">
      {properties.map((p, i) => (
        <div key={uuidv4()} className="descriptivePlate__line">
          <div className="descriptivePlate__label">{p.label}</div>
          <div className="descriptivePlate__value">{p.value}</div>
          {i === 0 && isDefined(contextActions) && (
            <ContextActions className="descriptivePlate__contextActions" actions={contextActions} />
          )}
        </div>
      ))}
    </div>
  );
}
