# Security policy

## Supported versions

Until the first stable release, security fixes are applied to the latest release and the default branch.

## Reporting a vulnerability

Use GitHub private vulnerability reporting when available, or email `andrew.giessel@gmail.com`. Please do not include live credentials, private prompts, or sensitive tool output in a public issue.

Useful reports include the affected version, impact, minimal reproduction, and whether the issue involves local spool data, authentication headers, destination isolation, or cross-tenant delivery.

## Security model

`gptel-otel` may process sensitive prompts, model responses, tool arguments/results, and delegated-agent tasks. Full payload capture is disabled by default. When enabled:

- data is durably spooled as plaintext before delivery;
- the spool directory and files are restricted to the current user where the platform supports Unix file modes;
- credentials and dynamic HTTP headers are not written to the spool;
- non-local collectors should use HTTPS;
- queue destination identity prevents automatic delivery to a changed tenant or project;
- delivery is at least once, so a crash after remote acceptance can produce duplicate spans.

Users are responsible for configuring backend access, retention, regional storage, and payload capture in accordance with their data policies.
