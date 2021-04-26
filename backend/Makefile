# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: CC0-1.0

.PHONY: build test clean

STACK_BUILD_OPTIONS = --haddock --fast --test --bench --no-run-tests --no-run-benchmarks
CABAL_BUILD_OPTIONS = --enable-tests --enable-benchmarks -O0
STACK_TEST_OPTIONS = --haddock --fast --test
# Addtional (specified by user) options passed to test executable.
# If you use double quotes there, please be careful: with `stack` you need to
# escape them while we `cabal` you don't.
# Examples:
# TEST_ARGUMENTS='--dry-run --no-color --match \"Valid tables\"' EDNA_USE_CABAL=0 make test
# TEST_ARGUMENTS='--dry-run --no-color --match "Valid tables"' EDNA_USE_CABAL=1 make test
TEST_ARGUMENTS ?=
# Like TEST_ARGUMENTS, but for the `run` target.
RUN_ARGUMENTS ?=
# Packages to apply the command (build, test, etc.) for.
PACKAGE = edna

ifeq (${EDNA_USE_CABAL},1)
	call_build = cabal new-build $(CABAL_BUILD_OPTIONS)
	call_build_no_watch = cabal new-build $(CABAL_BUILD_OPTIONS)
	call_test = cabal new-run $(PACKAGE):edna-test $(CABAL_BUILD_OPTIONS) -- \
		--color $(TEST_ARGUMENTS)
	call_server = cabal new-run $(PACKAGE):edna-server $(CABAL_BUILD_OPTIONS) -- \
		$(RUN_ARGUMENTS)
	call_clean = cabal new-clean
else
	call_build = stack build $(STACK_BUILD_OPTIONS) --file-watch $(PACKAGE)
	call_build_no_watch = stack build $(STACK_BUILD_OPTIONS) $(PACKAGE)
	call_test = stack build $(STACK_TEST_OPTIONS) $(PACKAGE) \
		--test-arguments "--color $(TEST_ARGUMENTS)"
	call_server = stack run edna-server -- \
		$(RUN_ARGUMENTS)
	call_clean = stack clean $(PACKAGE)
endif

# Build everything (including tests and benchmarks) with development options.
build:
	$(call call_build)

# Run tests in all packages which have them.
test:
	# We build everything first because setup-test-db creates a temporary DB
	# that doesn't live long.
	$(call call_build_no_watch)
	export TEST_PG_CONN_STRING="`scripts/setup-test-db.sh`" && cd ../analysis && \
		poetry install && poetry run bash -c "cd ../backend && $(call call_test)"

# Run edna-server inside `poetry shell` making sure Python analysis will work.
# If you want to pass additional arguments, use `RUN_ARGUMENTS` env variable.
run:
	cd ../analysis && poetry install && \
		poetry run bash -c "cd ../backend && $(call call_server)"

clean:
	$(call call_clean)
