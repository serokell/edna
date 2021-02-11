# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: CC0-1.0

.PHONY: build test clean

STACK_BUILD_OPTIONS = --haddock --fast --test --bench --no-run-tests --no-run-benchmarks --file-watch
CABAL_BUILD_OPTIONS = --enable-tests --enable-benchmarks -O0
STACK_TEST_OPTIONS = --haddock --fast --test
# Addtional (specified by user) options passed to test executable
TEST_ARGUMENTS ?= ""
# Packages to apply the command (build, test, etc.) for.
PACKAGE = edna

ifeq (${EDNA_USE_CABAL},1)
	call_build = cabal new-build $(CABAL_BUILD_OPTIONS)
	call_test = cabal new-run enda:edna-test $(CABAL_BUILD_OPTIONS) -- --color always $(TEST_ARGUMENTS)
	call_clean = cabal new-clean
else
	call_build = stack build $(STACK_BUILD_OPTIONS) $(PACKAGE)
	call_test = stack build $(STACK_TEST_OPTIONS) $(PACKAGE) \
		--test-arguments "--color always $(TEST_ARGUMENTS)" $(STACK_DEV_TEST_OPTIONS)
	call_clean = stack clean $(PACKAGE)
endif

# Build everything (including tests and benchmarks) with development options.
build:
	$(call call_build)

# Run tests in all packages which have them.
test:
	$(call call_test)

clean:
	$(call call_clean)
