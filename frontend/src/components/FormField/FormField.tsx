import React from "react";
import "./FormField.scss";
import cn from "classnames";
import { ErrorMessage, Field } from "formik";
import { FieldInputProps } from "formik/dist/types";

interface FormFieldProps<V> {
  name: string;
  label: string;
  // eslint-disable-next-line react/require-default-props
  className?: string;
  children: (field: FieldInputProps<V>) => React.ReactNode;
}

function FormField<V>({ name, label, children, className }: FormFieldProps<V>) {
  return (
    <div className={cn("formField", className)}>
      <div className="formField__label">
        {label}
        <span className="formField__error">
          <ErrorMessage name={name} />
        </span>
      </div>
      <Field name={name}>{({ field }: { field: FieldInputProps<V> }) => children(field)}</Field>
    </div>
  );
}

export default FormField;
