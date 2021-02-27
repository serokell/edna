import React, { FunctionComponent, ReactElement } from "react";
import "./FormField.scss";
import cn from "classnames";

interface FormFieldProps {
  label: string;
  className?: string;
  control: React.ReactNode;
}

const FormField: FunctionComponent<FormFieldProps> = ({
  label,
  control,
  className,
}): ReactElement => {
  return (
    <div className={cn("formField", className)}>
      <div className="formField__label">{label}</div>
      {control}
    </div>
  );
};

export default FormField;
