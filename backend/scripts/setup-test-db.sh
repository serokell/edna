#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

# Setup temporary DB for testing.
# This is a helper script for the Makefile we are using.
# It prints PostgreSQL connection string that we then put into a certain env var.
# pg_tmp shutdowns and cleans up automatically after a period of inactivity,
# so we don't need to clean up manually.

# Dependencies:
# * Postgres server
# * pg_tmp (http://eradman.com/ephemeralpg/)

set -e -o pipefail

if ! which initdb > /dev/null; then
    echo -e "Command 'initdb' not found.\nConsider adding '/usr/lib/postgresql/VERSION/bin/' to your PATH."
    exit 2
fi

# Running a server in background.
# Set log level to warning to avoid polluting NOTICE messages in tests.
echo "$(pg_tmp)&options=-c%20client_min_messages\%3DWARNING"
