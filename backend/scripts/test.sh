#!/usr/bin/env bash

# Runs blnd-tender tests.
# All passed parameters go to --test-arguments parameter of the test executable.

# Dependencies:
# * Postgres server
# * pg_tmp (http://eradman.com/ephemeralpg/)

# Prerequisites:
# * Specify your database user name via 'PGUSER' variable; default user name is your OS username.
#   If you haven't created a database user yet, run
#   > sudo -u postgres createuser --interactive --pwprompt
# * If your database user's password is not empty, specify it in .pgpass file
#   (see https://www.postgresql.org/docs/9.3/libpq-pgpass.html).

set -e -o pipefail

if ! which initdb > /dev/null; then
    echo -e "Command 'initdb' not found.\nConsider adding '/usr/lib/postgresql/VERSION/bin/' to your PATH."
    exit 2
fi

# A temporal server does not live long (60 sec by default), building tests first.
stack test --no-run-tests

# Running a server in background.
# Set log level to warning to avoid polluting NOTICE messages in tests.
conn_str="$(pg_tmp)&options=-c%20client_min_messages\%3DWARNING"

# Executing tests.
# This thing wraps every argument into double quotes because that's what stack wants :/
args=( "${@/#/\"}" ); args=( "${args[@]/%/\"}" )
TEST_PG_CONN_STRING="$conn_str" stack test --test-arguments "${args[*]}"

# pg_tmp shutdowns and cleans up automatically after a period of inactivity
