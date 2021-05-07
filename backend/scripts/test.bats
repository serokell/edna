#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

load 'test_helpers/bats-support/load'
load 'test_helpers/bats-assert/load'

function teardown() {
  unset EDNA_API_LISTEN
  unset EDNA_API_SERVE_DOCS
  unset EDNA_DB_MAX_CONNECTIONS
  unset EDNA_DB_INITIALISATION_MODE
  unset EDNA_DB_INITIALISATION_INIT_SCRIPT
  unset EDNA_LOGGING
  unset POSTGRES_HOST
  unset POSTGRES_PORT
  unset POSTGRES_USER
  unset POSTGRES_PASSWORD
  unset POSTGRES_DB
  unset EDNA_MDE_HOST
}

@test "Passing wrong EDNA_API_LISTEN CLA causes error" {
  run edna-server --listen-addr "this_is_not_host_and_port"

  assert_failure
  assert_output --partial "EDNA_API_LISTEN"
}

@test "Setting wrong EDNA_API_LISTEN env causes error" {
  run EDNA_API_LISTEN="this_is_not_host_and_port" edna-server

  assert_failure
  assert_output --partial "EDNA_API_LISTEN"
}


@test "Setting wrong EDNA_API_SERVE_DOCS env causes error" {
  run EDNA_API_SERVE_DOCS="this_is_not_bool" edna-server

  assert_failure
  assert_output --partial "EDNA_API_SERVE_DOCS"
}


@test "Passing wrong EDNA_DB_MAX_CONNECTIONS CLA causes error" {
  run edna-server --max-connections "this_is_not_a_number"

  assert_failure
  assert_output --partial "EDNA_DB_MAX_CONNECTIONS"
}

@test "Setting wrong EDNA_DB_MAX_CONNECTIONS env causes error" {
  run EDNA_DB_MAX_CONNECTIONS="this_is_not_a_number" edna-server

  assert_failure
  assert_output --partial "EDNA_DB_MAX_CONNECTIONS"
}


@test "Passing wrong EDNA_DB_INITIALISATION_MODE CLA causes error" {
  run edna-server -init-mode "wrong"

  assert_failure
  assert_output --partial "EDNA_DB_INITIALISATION_MODE"
}

@test "Setting wrong EDNA_DB_INITIALISATION_MODE env causes error" {
  run EDNA_DB_INITIALISATION_MODE="wrong" edna-server

  assert_failure
  assert_output --partial "EDNA_DB_INITIALISATION_MODE"
}


@test "Setting wrong POSTGRES_PORT env causes error" {
  run POSTGRES_PORT="this_is_not_a_number" edna-server

  assert_failure
  assert_output --partial "POSTGRES_PORT"
}


@test "Passing wrong EDNA_LOGGING CLA causes error" {
  run edna-server --logging wrong_logging_level

  assert_failure
  assert_output --partial "EDNA_LOGGING"
}

@test "Setting wrong EDNA_LOGGING env causes error" {
  run EDNA_LOGGING="wrong" edna-server

  assert_failure
  assert_output --partial "EDNA_LOGGING"
}


@test "Passing EDNA_DB_INITIALISATION_MODE CLA without EDNA_DB_INITIALISATION_INIT_SCRIPT does not change config" {
  run edna-server --init-mode "enable" --dump-config

  assert_success
  assert_line --index 1 "api:"
  assert_line --index 2 "  listen-addr: '*:9000'"
  assert_line --index 3 "  serve-docs: true"
  assert_line --index 4 "logging: prod"
  assert_line --index 5 "db:"
  assert_line --index 6 "  initialisation: null"
  assert_line --index 7 "  conn-string: host=/run/postgresql dbname=edna"
  assert_line --index 8 "  max-connections: 200"
}

@test "Passing EDNA_DB_INITIALISATION_MODE ENV without EDNA_DB_INITIALISATION_INIT_SCRIPT does not change config" {
  EDNA_DB_INITIALISATION_MODE="enable"
  run edna-server --dump-config

  assert_success
  assert_line --index 1 "api:"
  assert_line --index 2 "  listen-addr: '*:9000'"
  assert_line --index 3 "  serve-docs: true"
  assert_line --index 4 "logging: prod"
  assert_line --index 5 "db:"
  assert_line --index 6 "  initialisation: null"
  assert_line --index 7 "  conn-string: host=/run/postgresql dbname=edna"
  assert_line --index 8 "  max-connections: 200"
}


@test "Passing EDNA_DB_INITIALISATION_INIT_SCRIPT CLA without EDNA_DB_INITIALISATION_MODE does not change config" {
  run edna-server --init-script "./init.sql" --dump-config

  assert_success
  assert_line --index 1 "api:"
  assert_line --index 2 "  listen-addr: '*:9000'"
  assert_line --index 3 "  serve-docs: true"
  assert_line --index 4 "logging: prod"
  assert_line --index 5 "db:"
  assert_line --index 6 "  initialisation: null"
  assert_line --index 7 "  conn-string: host=/run/postgresql dbname=edna"
  assert_line --index 8 "  max-connections: 200"
}

@test "Passing EDNA_DB_INITIALISATION_INIT_SCRIPT ENV without EDNA_DB_INITIALISATION_MODE does not change config" {
  EDNA_DB_INITIALISATION_INIT_SCRIPT="./init.sql"
  run edna-server --dump-config

  assert_success
  assert_line --index 1 "api:"
  assert_line --index 2 "  listen-addr: '*:9000'"
  assert_line --index 3 "  serve-docs: true"
  assert_line --index 4 "logging: prod"
  assert_line --index 5 "db:"
  assert_line --index 6 "  initialisation: null"
  assert_line --index 7 "  conn-string: host=/run/postgresql dbname=edna"
  assert_line --index 8 "  max-connections: 200"
}


@test "Passing CLAs modifes config" {
  run edna-server --listen-addr "127.0.0.1:80" \
                  --serve-docs \
                  --conn-string "host=localhost dbname=edna" \
                  --max-connections 1 \
                  --init-mode "enable" \
                  --init-script "/init.sql" \
                  --logging dev \
                  --mde-host "https://mde.edna/list" \
                  --dump-config

  assert_success
  assert_line --index 1 "api:"
  assert_line --index 2 "  listen-addr: 127.0.0.1:80"
  assert_line --index 3 "  serve-docs: true"
  assert_line --index 4 "logging: dev"
  assert_line --index 5 "db:"
  assert_line --index 6 "  initialisation:"
  assert_line --index 7 "    init-script: /init.sql"
  assert_line --index 8 "    mode: enable"
  assert_line --index 9 "  conn-string: host=/run/postgresql dbname=edna host=localhost dbname=edna"
  assert_line --index 10 "  max-connections: 1"
  assert_line --index 11 "mde-host: https://mde.edna/list"
}

@test "Passing envs modifes config" {
  export EDNA_API_LISTEN_ADDR="127.0.0.1:8080"
  export EDNA_API_SERVE_DOCS=false
  export EDNA_DB_MAX_CONNECTIONS=3
  export EDNA_DB_INITIALISATION_MODE="enable-with-drop"
  export EDNA_DB_INITIALISATION_INIT_SCRIPT="./init.sql"
  export POSTGRES_HOST="127.0.0.13"
  export POSTGRES_PORT=1234
  export POSTGRES_USER="test"
  export POSTGRES_PASSWORD="pswd"
  export POSTGRES_DB="db"
  export EDNA_LOGGING="nothing"
  export EDNA_MDE_HOST="https://mde.edna"

  run edna-server --dump-config

  assert_success
  assert_line --index 1 "api:"
  assert_line --index 2 "  listen-addr: 127.0.0.1:8080"
  assert_line --index 3 "  serve-docs: false"
  assert_line --index 4 "logging: nothing"
  assert_line --index 5 "db:"
  assert_line --index 6 "  initialisation:"
  assert_line --index 7 "    init-script: ./init.sql"
  assert_line --index 8 "    mode: enable-with-drop"
  assert_line --index 9 "  conn-string: host=/run/postgresql dbname=edna host=127.0.0.13 port=1234 dbname=db"
  assert_line --index 10 "    user=test password=pswd"
  assert_line --index 11 "  max-connections: 3"
  assert_line --index 12 "mde-host: https://mde.edna"
}

@test "CLAs has higher priority then envs" {
  export EDNA_API_LISTEN_ADDR="127.0.0.1:8080"
  export EDNA_API_SERVE_DOCS=false
  export EDNA_DB_MAX_CONNECTIONS=3
  export EDNA_DB_INITIALISATION_MODE="enable-with-drop"
  export EDNA_DB_INITIALISATION_INIT_SCRIPT="./init.sql"
  export POSTGRES_HOST="127.0.0.13"
  export POSTGRES_PORT=1234
  export POSTGRES_USER="test"
  export POSTGRES_PASSWORD="pswd"
  export POSTGRES_DB="db"
  export EDNA_LOGGING="nothing"
  export EDNA_MDE_HOST="https://mde.edna"

  run edna-server --listen-addr "127.0.0.1:80" \
                  --serve-docs \
                  --conn-string "host=localhost dbname=edna" \
                  --max-connections 1 \
                  --init-mode "enable" \
                  --init-script "/init.sql" \
                  --logging dev \
                  --mde-host "https://mde.edna/list" \
                  --dump-config

  assert_success
  assert_line --index 1 "api:"
  assert_line --index 2 "  listen-addr: 127.0.0.1:80"
  assert_line --index 3 "  serve-docs: true"
  assert_line --index 4 "logging: dev"
  assert_line --index 5 "db:"
  assert_line --index 6 "  initialisation:"
  assert_line --index 7 "    init-script: /init.sql"
  assert_line --index 8 "    mode: enable"
  assert_line --index 9 "  conn-string: host=/run/postgresql dbname=edna host=127.0.0.13 port=1234 dbname=db"
  assert_line --index 10 "    user=test password=pswd host=localhost dbname=edna"
  assert_line --index 11 "  max-connections: 1"
  assert_line --index 12 "mde-host: https://mde.edna/list"
}
