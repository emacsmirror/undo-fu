# SPDX-License-Identifier: GPL-2.0-or-later

define HELP_TEXT

- test, watch_test
  Runs all tests.

Environment Variables:

- EMACS_BIN
  The command used to run Emacs, defaults to "emacs"

endef
# HELP_TEXT (end)

# Needed for when tests are run from another directory: `make -C ./path/to/tests`.
BASE_DIR := $(CURDIR)

# Default Emacs binary
EMACS_BIN ?= emacs

EL_FILES := \
	./undo-fu.el \
	./tests/undo-fu_tests.el

# Additional files to watch (can be overridden)
EXTRA_WATCH_FILES ?=

# -----------------------------------------------------------------------------
# Help for build targets

export HELP_TEXT
.PHONY: help
help: FORCE
	@echo "$$HELP_TEXT"


# -----------------------------------------------------------------------------
# Tests

.PHONY: test
test: FORCE
	@cd "$(BASE_DIR)" && \
	python ./tests/undo-fu_tests.py

.PHONY: watch_test
watch_test: FORCE
	@cd "$(BASE_DIR)" && \
	bash -c "while true; do \
		inotifywait -e close_write $(EL_FILES); \
		tput clear && make test; \
	done"

FORCE:
