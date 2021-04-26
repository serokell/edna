// SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import React from "react";
import { ErrorBoundary, FallbackProps } from "react-error-boundary";

interface ErrorPlaceholderProps {
  children: React.ReactNode;
}

function ErrorFallback({ error }: FallbackProps) {
  return (
    <div role="alert">
      <p>Something went wrong:</p>
      <pre>{error.message}</pre>
    </div>
  );
}

export function ErrorPlaceholder({ children }: ErrorPlaceholderProps): React.ReactElement {
  return <ErrorBoundary FallbackComponent={ErrorFallback}>{children}</ErrorBoundary>;
}
