EMACS ?= emacs
GPTEL_DIR ?=
GPTEL_AGENT_DIR ?=
ELPA_DIR ?=
DEPS = $(if $(strip $(GPTEL_DIR)),-L $(GPTEL_DIR),) \
	$(if $(strip $(GPTEL_AGENT_DIR)),-L $(GPTEL_AGENT_DIR),)
PACKAGE_INIT = $(if $(strip $(ELPA_DIR)),--eval "(progn (require 'package) (setq package-user-dir (expand-file-name \"$(ELPA_DIR)\")) (package-initialize))",)
BATCH = $(EMACS) -Q --batch $(PACKAGE_INIT) -L . $(DEPS)

.PHONY: test compile clean

test:
	$(BATCH) -L test -l test/gptel-otel-core-test.el -l test/gptel-otel-transport-test.el -l test/gptel-otel-test.el -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile gptel-otel-core.el gptel-otel-transport.el gptel-otel.el

lint:
	$(BATCH) -l package-lint -f package-lint-batch-and-exit gptel-otel.el

clean:
	rm -f *.elc test/*.elc
