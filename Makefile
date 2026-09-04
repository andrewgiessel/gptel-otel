EMACS ?= emacs
GPTEL_DIR ?= $(HOME)/.local/state/emacs/elpa/gptel-20260805.313
GPTEL_AGENT_DIR ?= $(HOME)/.local/state/emacs/elpa/gptel-agent-20260717.506
BATCH = $(EMACS) -Q --batch -L . -L $(GPTEL_DIR) -L $(GPTEL_AGENT_DIR)

.PHONY: test compile clean

test:
	$(BATCH) -L test -l test/gptel-otel-core-test.el -l test/gptel-otel-transport-test.el -l test/gptel-otel-test.el -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile gptel-otel-core.el gptel-otel-transport.el gptel-otel.el

clean:
	rm -f *.elc test/*.elc
