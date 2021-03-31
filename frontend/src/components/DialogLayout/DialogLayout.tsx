import React from "react";
import { Form, Formik, FormikConfig } from "formik";
import CrossSvg from "../../assets/svg/cross.svg";
import "./DialogLayout.scss";
import cn from "../../utils/bemUtils";

export interface DialogLayoutProps<T> {
  title: string;
  description?: React.ReactNode;
  children?: React.ReactNode;
  footer?: React.ReactNode;
  onClose: () => void;
  dialogClass: string;

  formik?: FormikConfig<T>;
}

export function DialogLayout<T = any>({
  title,
  description,
  children,
  footer,
  onClose,
  dialogClass,
  formik,
}: DialogLayoutProps<T>): React.ReactElement {
  const dialogCls = cn(dialogClass);
  return (
    <div className="dialogBackground" onClick={onClose}>
      <div
        className={dialogClass}
        onClick={e => {
          e.stopPropagation();
        }}
      >
        <div className="dialogHeader">
          <span className={dialogCls("title")}>{title}</span>
          <div className="closeDialogBtn" onClick={onClose}>
            <CrossSvg />
          </div>
        </div>

        {description && <div className={dialogCls("description")}>{description} </div>}
        <div className={dialogCls("spacer")} />

        {formik ? (
          <Formik<T> {...formik}>
            <Form>
              {children}
              {footer && <div className={dialogCls("footer")}>{footer}</div>}
            </Form>
          </Formik>
        ) : (
          <>
            {children}
            {footer && <div className={dialogCls("footer")}>{footer}</div>}
          </>
        )}
      </div>
    </div>
  );
}
