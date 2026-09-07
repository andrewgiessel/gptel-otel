;;; run-tests.el --- Isolated batch test entry point -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Exclude the optional package even on developer machines with it installed.
;; gptel has no non-built-in dependencies, so a base run needs only its path.
(unless (equal (getenv "WITH_AGENT") "1")
  (setq load-path
        (cl-remove-if
         (lambda (dir)
           (and dir (or (file-exists-p (expand-file-name "gptel-agent.el" dir))
                        (file-exists-p (expand-file-name "gptel-agent.elc" dir)))))
         load-path))
  (when (or (featurep 'gptel-agent) (locate-library "gptel-agent"))
    (error "Base tests must not have gptel-agent available")))

(require 'gptel-otel)
(when (featurep 'gptel-agent)
  (error "Loading gptel-otel unexpectedly loaded gptel-agent"))

;; All suites, including mode activation tests, must be unable to replay the
;; user's queue or make exporter requests.
(let* ((spool (make-temp-file "gptel-otel-suite-" t))
       (gptel-otel-spool-directory spool)
       (gptel-otel-delivery-function (lambda (&rest _) nil))
       (gptel-otel--delivery-timer nil)
       (gptel-otel--watchdog-timer nil)
       (exit-code 1))
  (unwind-protect
      (progn
        (when (equal (getenv "WITH_AGENT") "1")
          ;; Verify the real late-load hook before the rest of the tests.
          (gptel-otel-mode 1)
          (unless (plist-get (gptel-otel-compatibility-status) :base-active)
            (error "Base adapter is incompatible"))
          (require 'gptel-agent)
          (unless (plist-get (gptel-otel-compatibility-status) :agent-active)
            (error "Late gptel-agent load did not activate compatible tracing"))
          (gptel-otel-mode -1))
        (dolist (file '("gptel-otel-core-test" "gptel-otel-transport-test"
                        "gptel-otel-test" "gptel-otel-adapter-test"
                        "gptel-otel-integration-test" "gptel-otel-langfuse-mcp-test"))
          (load file nil t))
        (let ((stats (ert-run-tests-batch t)))
          (setq exit-code (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))
    (gptel-otel-mode -1)
    (dolist (timer (list gptel-otel--delivery-timer gptel-otel--watchdog-timer))
      (when (timerp timer) (cancel-timer timer)))
    (delete-directory spool t))
  (kill-emacs exit-code))
;;; run-tests.el ends here
