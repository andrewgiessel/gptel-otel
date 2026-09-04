# Contributing

Contributions are welcome. `gptel-otel` instruments private gptel and gptel-agent lifecycle functions, so compatibility and failure containment are part of every change.

## Development setup

Install current development versions of gptel and gptel-agent, then run:

```sh
make test GPTEL_DIR=/path/to/gptel GPTEL_AGENT_DIR=/path/to/gptel-agent
make compile GPTEL_DIR=/path/to/gptel GPTEL_AGENT_DIR=/path/to/gptel-agent
```

Before submitting a change, also run `checkdoc` and `package-lint` when available.

## Expectations

- Preserve normal gptel control flow when telemetry fails.
- Keep private lifecycle seams signature-guarded.
- Add focused ERT coverage for behavior changes.
- Never silently truncate or discard captured payloads.
- Never persist authentication headers or credentials.
- Preserve unrelated queue entries and backward compatibility where practical.
- Document changes to public customization variables, spool formats, or telemetry semantics.

## Compatibility changes

When gptel or gptel-agent changes a private seam:

1. identify the exact upstream version or commit;
2. update the guard and integration together;
3. add a regression test for the new lifecycle;
4. verify requests, tools, and agents still complete when tracing is enabled;
5. update the tested-version statement and release notes.

## Security reports

Do not open a public issue for a vulnerability that could expose prompts, tool data, credentials, or cross-tenant telemetry. Use GitHub's private vulnerability reporting for the repository, or contact the maintainer privately at `andrew.giessel@gmail.com`.
