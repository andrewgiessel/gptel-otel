# Changelog

All notable changes to this project will be documented here.

The project follows semantic versioning before and after 1.0. During the 0.x series, minor releases may change telemetry semantics or configuration APIs; persisted spool-format changes will be called out explicitly.

## Unreleased

- Prepare the package for installation against stock gptel and gptel-agent.
- Make full payload capture opt-in by default.
- Add public installation, privacy, compatibility, and queue-operation documentation.
- Add byte-aware multi-request OTLP export and Langfuse canonical-content handling.

## 0.3.0

- Initial private development release with Langfuse and generic OTLP/HTTP profiles.
- Trace gptel requests, model generations, tools, and delegated agents.
- Add durable private spooling, retries, destination isolation, and partial-success handling.
