# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: CC0-1.0

.PHONY: build test clean

STACK_BUILD_OPTIONS = --haddock --fast --test --bench --no-run-tests --no-run-benchmarks --file-watch
# Options for tests
STACK_TEST_OPTIONS = --haddock --fast --test
# Addtional (specified by user) options passed to test executable
TEST_ARGUMENTS ?= ""
# Packages to apply the command (build, test, etc.) for.
PACKAGE = edna

# Build everything (including tests and benchmarks) with development options.
build:
	stack build $(STACK_BUILD_OPTIONS) $(PACKAGE)

# Run tests in all packages which have them.
test:
	stack build $(STACK_TEST_OPTIONS) $(PACKAGE) \
		--test-arguments "--color always $(TEST_ARGUMENTS)" $(STACK_DEV_TEST_OPTIONS)

clean:
	stack clean $(PACKAGE)
