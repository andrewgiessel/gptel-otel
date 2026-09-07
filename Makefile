EMACS ?= emacs
GPTEL_DIR ?=
GPTEL_AGENT_DIR ?=
ELPA_DIR ?=
WITH_AGENT ?= 0
LOAD_PREFER_NEWER ?= nil
export WITH_AGENT
DEPS = $(if $(strip $(GPTEL_DIR)),-L $(GPTEL_DIR),) \
	$(if $(strip $(GPTEL_AGENT_DIR)),-L $(GPTEL_AGENT_DIR),)
PACKAGE_INIT = $(if $(strip $(ELPA_DIR)),--eval "(progn (require 'package) (setq package-user-dir (expand-file-name \"$(ELPA_DIR)\")) (package-initialize))",)
BATCH = $(EMACS) -Q --batch $(PACKAGE_INIT) -L . $(DEPS) --eval "(setq load-prefer-newer $(LOAD_PREFER_NEWER))"

.PHONY: test test-base test-agent compile clean lint

test:
	$(BATCH) -L test -l test/run-tests.el

test-base:
	$(MAKE) test WITH_AGENT=0

test-agent:
	$(MAKE) test WITH_AGENT=1

compile: LOAD_PREFER_NEWER = t
compile:
	$(BATCH) -f batch-byte-compile gptel-otel-core.el gptel-otel-transport.el gptel-otel.el gptel-otel-adapter.el gptel-otel-agent-adapter.el
	$(BATCH) -f batch-byte-compile gptel-otel-langfuse-mcp.el

lint:
	$(BATCH) -l package-lint -f package-lint-batch-and-exit gptel-otel.el

clean:
	rm -f *.elc test/*.elc
