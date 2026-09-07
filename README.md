# gptel-otel

OpenTelemetry tracing for stock [gptel](https://github.com/karthink/gptel), with optional [gptel-agent](https://github.com/karthink/gptel-agent) support. No personal configuration or patched gptel is required.

`gptel-otel` records one trace per gptel request, with nested observations for model generations, tool executions, and delegated agents. Traces are durably spooled before asynchronous OTLP/HTTP JSON delivery. Langfuse is the first polished backend; generic OTLP/HTTP collectors are supported through backend profiles.

## Status

This package is pre-1.0. It requires Emacs 29.1+ and gptel 0.9.9+; that minimum version is not a guarantee of compatibility with every upstream revision. The tested baseline is gptel `fc6963634af2` (package `20260805.313`) and, optionally, gptel-agent `e833bcaf617b` (package `20260717.506`). CI is configured to test both pinned baseline and current MELPA dependencies, with and without gptel-agent.

Exact lifecycle tracing still uses private upstream functions through compatibility adapters. Signature mismatches disable dependent capabilities together: required request/generation tracing, optional tool tracing, and optional agent correlation. Signature checks cannot detect every upstream behavior change; lifecycle contract tests provide additional coverage. Recheck compatibility when upgrading.

`M-x gptel-otel-compatibility-status` reports compatible versus active capabilities and the first mismatched function, if any. Missing optional gptel-agent is normal, not an error.

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

Install gptel, then clone this repository and add it to `load-path`, or use a package manager that supports Git repositories. gptel-agent is not required.

```elisp
(add-to-list 'load-path "/path/to/gptel-otel")
(require 'gptel-otel)
```

Configure a backend, then enable the global mode.

### Optional subagent tracing

Stock gptel provides model requests and tool execution. gptel-agent adds tools, agent presets, and an asynchronous `Agent` tool that starts another gptel request and returns its result to the parent.

There is one shared request-tracing path. When gptel-agent is loaded, the optional adapter adds the parent–child association between an `Agent` invocation and its child request; it does not replace or duplicate base tracing. Compatible agent support activates automatically, including when gptel-agent loads after `gptel-otel-mode` is enabled. gptel-otel never loads gptel-agent for you.

Custom tools can also launch child gptel requests without gptel-agent. Those requests are traced, but exact subagent parentage requires an integration for that implementation.

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

Run tests in fresh `emacs -Q --batch` processes. The base run excludes gptel-agent from the load path; the agent run uses an installed package directory containing gptel, gptel-agent, and its dependencies:

```sh
make test-base GPTEL_DIR=/path/to/gptel
make test-agent ELPA_DIR=/path/to/elpa

make compile GPTEL_DIR=/path/to/gptel
```

`make test` defaults to the base configuration; `WITH_AGENT=1` selects agent support. Tests use temporary queues and simulated delivery. The suite includes real stock-gptel request/FSM execution with simulated transport (streaming, synchronous/asynchronous tools, concurrent requests, errors, and aborts), synthetic agent-lineage fixtures, late agent loading, compatibility guards, and durable transport tests. It does not make live model or collector requests.

## License

MIT
