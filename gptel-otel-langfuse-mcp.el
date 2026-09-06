;;; gptel-otel-langfuse-mcp.el --- Optional Langfuse MCP tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Giessel
;; Author: Andrew Giessel <andrew.giessel@gmail.com>
;; Maintainer: Andrew Giessel <andrew.giessel@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/andrewgiessel/gptel-otel

;;; Commentary:
;; Opt-in registration of Langfuse's read-only MCP tools for gptel.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel-otel-transport)

(defvar gptel--known-tools)
(defvar mcp-server-connections)
(defvar gptel-otel-langfuse-mcp-mode)
(declare-function gptel-make-tool "gptel")
(declare-function gptel-agent-update "gptel-agent")
(declare-function mcp-connect-server "mcp")
(declare-function mcp-make-text-tool "mcp")
(declare-function mcp-stop-server "mcp")

(defconst gptel-otel-langfuse-mcp--server-name "langfuse")
(defconst gptel-otel-langfuse-mcp--category "langfuse-read")
(defconst gptel-otel-langfuse-mcp--tool-names
  '("listObservations" "getObservation" "getObservationFieldSchema"
    "getObservationFilterSchema" "getObservationFilterValues" "queryMetrics"
    "getMetricsSchema")
  "Read-only Langfuse MCP tool names exposed by this module.")

(defcustom gptel-otel-langfuse-mcp-timeout 60
  "Seconds to wait for Langfuse MCP tool discovery."
  :type 'number :group 'gptel-otel)

(defvar gptel-otel-langfuse-mcp--generation 0)
(defvar gptel-otel-langfuse-mcp--connection nil)
(defvar gptel-otel-langfuse-mcp--discovery-timer nil)

(defun gptel-otel-langfuse-mcp--report (diagnostic)
  "Report a fixed, non-sensitive DIAGNOSTIC."
  (message "gptel-otel-langfuse-mcp: %s" diagnostic))

(defun gptel-otel-langfuse-mcp--base-url ()
  "Return the configured or legacy-derived Langfuse base URL, if any."
  (or gptel-otel-langfuse-base-url
      (when (and (stringp gptel-otel-endpoint)
                 (string-match
                  "\\`\\(https?://[^/]+\\)\\(?:/api/public/otel/v1/traces\\)/?\\'"
                  gptel-otel-endpoint))
        (match-string 1 gptel-otel-endpoint))))

(defun gptel-otel-langfuse-mcp--headers ()
  "Return fresh HTTP authorization headers, or nil without complete credentials."
  (when-let* ((keys (and gptel-otel-langfuse-auth-function
                          (funcall gptel-otel-langfuse-auth-function)))
              (public (car-safe keys))
              (secret (cdr-safe keys)))
    `(("Authorization" .
       ,(concat "Basic " (base64-encode-string (concat public ":" secret) t))))))

(defun gptel-otel-langfuse-mcp--cancel-discovery-timer ()
  (when (timerp gptel-otel-langfuse-mcp--discovery-timer)
    (cancel-timer gptel-otel-langfuse-mcp--discovery-timer))
  (setq gptel-otel-langfuse-mcp--discovery-timer nil))

(defun gptel-otel-langfuse-mcp--remove-tools ()
  "Remove this module's category without disturbing other categories."
  (when (boundp 'gptel--known-tools)
    (setq gptel--known-tools
          (assoc-delete-all gptel-otel-langfuse-mcp--category gptel--known-tools)))
  (when (fboundp 'gptel-agent-update) (gptel-agent-update)))

(defun gptel-otel-langfuse-mcp--forget-connection (connection)
  "Stop and remove CONNECTION only when it remains this module's connection."
  (when (and connection
             (boundp 'mcp-server-connections)
             (eq connection
                 (gethash gptel-otel-langfuse-mcp--server-name
                          mcp-server-connections)))
    (when (fboundp 'mcp-stop-server)
      (mcp-stop-server gptel-otel-langfuse-mcp--server-name))
    ;; `mcp-stop-server' marks a connection stopped but does not remhash it.
    (when (eq connection
              (gethash gptel-otel-langfuse-mcp--server-name
                       mcp-server-connections))
      (remhash gptel-otel-langfuse-mcp--server-name mcp-server-connections))))

(defun gptel-otel-langfuse-mcp--stop ()
  "Stop and forget this module's connection only."
  (cl-incf gptel-otel-langfuse-mcp--generation)
  (gptel-otel-langfuse-mcp--cancel-discovery-timer)
  (gptel-otel-langfuse-mcp--remove-tools)
  (gptel-otel-langfuse-mcp--forget-connection
   gptel-otel-langfuse-mcp--connection)
  (setq gptel-otel-langfuse-mcp--connection nil))

(defun gptel-otel-langfuse-mcp--current-p (generation connection)
  (and gptel-otel-langfuse-mcp-mode
       (= generation gptel-otel-langfuse-mcp--generation)
       (eq connection gptel-otel-langfuse-mcp--connection)
       (boundp 'mcp-server-connections)
       (eq connection
           (gethash gptel-otel-langfuse-mcp--server-name
                    mcp-server-connections))))

(defun gptel-otel-langfuse-mcp--fail (generation connection diagnostic)
  (when (gptel-otel-langfuse-mcp--current-p generation connection)
    (gptel-otel-langfuse-mcp--report diagnostic)
    (gptel-otel-langfuse-mcp--stop)
    (setq gptel-otel-langfuse-mcp-mode nil)))

(defun gptel-otel-langfuse-mcp--install-tools (generation connection tools)
  "Install allowed TOOLS only when CONNECTION belongs to the current attempt."
  (when (gptel-otel-langfuse-mcp--current-p generation connection)
    (condition-case nil
        (progn
          (gptel-otel-langfuse-mcp--cancel-discovery-timer)
          (let* ((available (mapcar (lambda (tool) (plist-get tool :name)) tools))
                 (missing (cl-set-difference gptel-otel-langfuse-mcp--tool-names available
                                             :test #'equal)))
            (if missing
                (gptel-otel-langfuse-mcp--fail
                 generation connection "tool discovery missing required read-only tools")
              (progn
                (setq gptel--known-tools
                      (assoc-delete-all gptel-otel-langfuse-mcp--category
                                        gptel--known-tools))
                (dolist (name gptel-otel-langfuse-mcp--tool-names)
                  (let ((spec (mcp-make-text-tool
                               gptel-otel-langfuse-mcp--server-name name t)))
                    (unless spec
                      (error "MCP tool unavailable"))
                    ;; gptel-make-tool registers a real (NAME . gptel-tool) entry.
                    (apply #'gptel-make-tool
                           (plist-put spec :category
                                      gptel-otel-langfuse-mcp--category))))
                (when (fboundp 'gptel-agent-update)
                  (gptel-agent-update))))))
      (error
       (gptel-otel-langfuse-mcp--fail
        generation connection "MCP tool discovery failed")))))

(defun gptel-otel-langfuse-mcp--start ()
  "Start a fresh optional Langfuse MCP connection."
  (let (connection)
    (condition-case nil
        (let ((mcp-ready (require 'mcp nil t))
              (gptel-ready (require 'gptel nil t)))
          (cond
           ((not (and mcp-ready gptel-ready))
            (gptel-otel-langfuse-mcp--report "missing required MCP or gptel dependency")
            (setq gptel-otel-langfuse-mcp-mode nil))
           ((not (gptel-otel-langfuse-mcp--base-url))
            (gptel-otel-langfuse-mcp--report "missing Langfuse base URL")
            (setq gptel-otel-langfuse-mcp-mode nil))
           ((gethash gptel-otel-langfuse-mcp--server-name mcp-server-connections)
            (gptel-otel-langfuse-mcp--report "Langfuse MCP server name is already in use")
            (setq gptel-otel-langfuse-mcp-mode nil))
           (t
            ;; Look up credentials exactly once for every enable/restart.
            (let ((headers (gptel-otel-langfuse-mcp--headers)))
              (if (not headers)
                  (progn
                    (gptel-otel-langfuse-mcp--report "missing Langfuse credentials")
                    (setq gptel-otel-langfuse-mcp-mode nil))
                (let* ((generation (cl-incf gptel-otel-langfuse-mcp--generation))
                       (url (concat (string-remove-suffix "/"
                                                          (gptel-otel-langfuse-mcp--base-url))
                                    "/api/public/mcp"))
                       error-pending pending-tools)
                  (mcp-connect-server
                         gptel-otel-langfuse-mcp--server-name
                         :url url :headers headers
                         :timeout gptel-otel-langfuse-mcp-timeout
                         :tools-callback
                         (lambda (callback-connection tools)
                           ;; mcp can invoke callbacks before returning the
                           ;; connection; defer those until identity is known.
                           (if connection
                               (gptel-otel-langfuse-mcp--install-tools
                                generation callback-connection tools)
                             (setq pending-tools (cons callback-connection tools))))
                         :error-callback
                         (lambda (_code _message)
                           (if connection
                               (gptel-otel-langfuse-mcp--fail
                                generation connection "MCP initialization failed")
                             (setq error-pending t))))
                  ;; The asynchronous API returns a startup timer, not the
                  ;; connection; the registry is the source of its identity.
                  (setq connection
                        (gethash gptel-otel-langfuse-mcp--server-name
                                 mcp-server-connections))
                  (if connection
                      (progn
                        (setq gptel-otel-langfuse-mcp--connection connection)
                        (setq gptel-otel-langfuse-mcp--discovery-timer
                              (run-at-time gptel-otel-langfuse-mcp-timeout nil
                                           (lambda ()
                                             (gptel-otel-langfuse-mcp--fail
                                              generation connection
                                              "MCP tool discovery timed out"))))
                        (cond
                         (error-pending
                          (gptel-otel-langfuse-mcp--fail
                           generation connection "MCP initialization failed"))
                         (pending-tools
                          (gptel-otel-langfuse-mcp--install-tools
                           generation (car pending-tools) (cdr pending-tools)))))
                    (gptel-otel-langfuse-mcp--report "MCP initialization failed")
                    (setq gptel-otel-langfuse-mcp-mode nil))))))))
      (error
       ;; A synchronous connect/auth error can register a connection before it
       ;; signals.  Remove only that still-current connection.
       (gptel-otel-langfuse-mcp--forget-connection
        (or connection
            (and (boundp 'mcp-server-connections)
                 (gethash gptel-otel-langfuse-mcp--server-name
                          mcp-server-connections))))
       (setq gptel-otel-langfuse-mcp--connection nil)
       (gptel-otel-langfuse-mcp--cancel-discovery-timer)
       (gptel-otel-langfuse-mcp--remove-tools)
       (gptel-otel-langfuse-mcp--report "MCP initialization failed")
       (setq gptel-otel-langfuse-mcp-mode nil)))))

;;;###autoload
(define-minor-mode gptel-otel-langfuse-mcp-mode
  "Globally expose Langfuse's read-only MCP tools to gptel.
This mode is opt-in and never affects tracing when disabled.  Enabling it again
restarts the connection and re-evaluates credentials."
  :global t :group 'gptel-otel
  (gptel-otel-langfuse-mcp--stop)
  (when gptel-otel-langfuse-mcp-mode
    ;; `--stop' invalidates old callbacks; this is the fresh attempt generation.
    (gptel-otel-langfuse-mcp--start)))

(provide 'gptel-otel-langfuse-mcp)
;;; gptel-otel-langfuse-mcp.el ends here.
