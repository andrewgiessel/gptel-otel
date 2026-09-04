# gptel-otel

OpenTelemetry tracing for [gptel](https://github.com/karthink/gptel) and [gptel-agent](https://github.com/karthink/gptel-agent).

`gptel-otel` records one trace per gptel request, with nested observations for model generations, tool executions, and delegated agents. Traces are durably spooled before asynchronous OTLP/HTTP JSON delivery. Langfuse is the first polished backend; generic OTLP/HTTP collectors are supported through backend profiles.

## Status

This package is pre-1.0 and depends on signature-guarded private gptel and gptel-agent lifecycle functions. It currently targets:

- Emacs 29.1 or later
- gptel 0.9.9 or later
- a current gptel-agent development snapshot compatible with the tested seams

If a private function signature changes, the affected instrumentation layer is disabled with a warning instead of changing gptel control flow. Compatibility still needs to be verified when upgrading gptel or gptel-agent.

## Trace model

```text
Session: one persistent conversation
└── Trace: one bounded gptel request/turn
    └── chat-turn
        ├── chat MODEL
        ├── execute_tool TOOL
        ├── execute_tool Agent
        │   └── invoke_agent AGENT-TYPE
        │       ├── chat MODEL
        │       └── execute_tool TOOL
        └── chat MODEL
```

The default metadata provider does not assign a session ID. Applications with durable conversations should set `gptel-otel-trace-metadata-function` and return a stable `:session-id`.

## Installation

Clone the repository and add it to `load-path`, or install it with a package manager that supports Git repositories. Ensure gptel-agent is installed first.

```elisp
(add-to-list 'load-path "/path/to/gptel-otel")
(require 'gptel-otel)
```

Load the package after gptel-agent, configure a backend, and then enable the global mode.

## Langfuse

Store the Langfuse project credentials in `auth-source`. For example, in `~/.authinfo.gpg` or a mode-`0600` `~/.authinfo`:

```text
machine us.cloud.langfuse.com port https login pk-lf-... password sk-lf-...
```

Configure the backend:

```elisp
(setq gptel-otel-backend-profile 'langfuse
      gptel-otel-langfuse-base-url "https://us.cloud.langfuse.com")

;; Full prompts, responses, tool arguments/results, and agent tasks are
;; deliberately opt-in because they may contain sensitive data.
(setq gptel-otel-capture-payloads t)

(gptel-otel-mode 1)
```

Langfuse Cloud regions use different base URLs. Supply the base URL for the region containing your project.

## Generic OTLP/HTTP

```elisp
(setq gptel-otel-backend-profile 'otlp-http
      gptel-otel-endpoint "http://localhost:4318/v1/traces"
      gptel-otel-generic-account-id "local-collector")

(gptel-otel-mode 1)
```

Dynamic headers, including Basic or Bearer authorization and tenant headers, can be supplied without writing credentials to the spool:

```elisp
(setq gptel-otel-headers-function
      (lambda ()
        `(("Authorization" . ,(concat "Bearer "
                                      (auth-source-pick-first-password
                                       :host "collector.example")))
          ("X-Tenant-ID" . "example"))))
```

Use HTTPS for non-local collectors when payload capture is enabled.

## Conversation metadata

The metadata callback receives the gptel request FSM and may return:

```elisp
(setq gptel-otel-trace-metadata-function
      (lambda (_fsm)
        (list :name "chat-turn"
              :session-id "persistent-conversation-id"
              :user-id nil
              :tags ["emacs" "gptel"]
              :metadata '(("project" . "example")))))
```

Names should be stable, low-cardinality operation labels. Put conversation titles, turn identifiers, project names, and similar values in metadata instead of the trace name.

## Privacy and local retention

`gptel-otel-capture-payloads` defaults to `nil`. When enabled, the package records complete logical inputs and outputs without truncation, including model requests, responses, tool arguments/results, and delegated-agent tasks/results.

Completed traces are written to `gptel-otel-spool-directory` before network delivery. The directory is created with mode `0700` and queue files with mode `0600`, but their contents are plaintext. The default location is:

```text
$XDG_STATE_HOME/gptel-otel/
```

or, if `XDG_STATE_HOME` is unset:

```text
~/.local/state/gptel-otel/
```

Review the data policy and backend access controls before enabling payload capture.

## Queue operations

- `M-x gptel-otel-flush` attempts delivery of pending entries.
- `M-x gptel-otel-replay` retries pending entries.
- `C-u M-x gptel-otel-replay` also resets safe permanent failures.
- `(gptel-otel-status)` returns bounded queue counts and the last delivery state.
- `M-x gptel-otel-migrate-legacy-queue` explicitly assigns destination identity to queue entries created by an older version.

Destination-mismatched and partial-success entries are not automatically replayed because doing so can disclose data to another tenant or duplicate spans already accepted by the receiver.

Delivery is at least once. A process crash after remote acceptance but before local deletion may cause a duplicate delivery. Stable trace and span IDs are preserved.

A trace may be split across several byte-bounded OTLP requests. Batch boundaries do not change trace IDs, span IDs, or parentage. An individually oversized span is retained locally as a permanent queue entry rather than silently truncated.

## Development

Run tests against package directories already installed on the machine:

```sh
make test \
  GPTEL_DIR=/path/to/gptel \
  GPTEL_AGENT_DIR=/path/to/gptel-agent

make compile \
  GPTEL_DIR=/path/to/gptel \
  GPTEL_AGENT_DIR=/path/to/gptel-agent
```

The test suite includes stock gptel request, generation, tool, and gptel-agent lifecycle fixtures; durable queue recovery; destination isolation; batching; partial-success handling; and failure containment.

## License

MIT
