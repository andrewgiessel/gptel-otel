;;; gptel-otel-agent-adapter.el --- Optional gptel-agent adapter  -*- lexical-binding: t; -*-

;; This module deliberately does not require gptel-agent: it is a passive
;; optional adapter activated by the feature's load hook.

(require 'cl-lib)
(require 'gptel-otel-adapter)

(defvar gptel-otel--agent-installed-advices nil)
(defvar gptel-otel--agent-adapter-capability nil)
(defvar gptel-otel--agent-bindings (make-hash-table :test #'eq))
(defvar gptel-otel--next-agent-execution nil)
(defvar gptel-otel--pending-agent nil)
(defvar gptel-otel--request-decision nil)
(defvar gptel-otel--request-decisions)

(declare-function gptel-otel--string-attr "gptel-otel")
(declare-function gptel-otel--set-semantic-attributes "gptel-otel")
(declare-function gptel-tool-name "gptel")
(declare-function gptel-otel--context "gptel-otel")
(declare-function gptel-otel--request-decision-for-fsm "gptel-otel")
(declare-function gptel-otel--new-context "gptel-otel")
(declare-function gptel-otel--mode-reconcile-prompt "gptel-otel")
(declare-function gptel-otel--observation-attributes "gptel-otel")
(declare-function gptel-otel--maybe-export "gptel-otel")
(declare-function gptel-otel-trace-start-span "gptel-otel-core")
(declare-function gptel-otel-trace-end-span "gptel-otel-core")
(declare-function gptel-otel-status-ok "gptel-otel-core")
(declare-function gptel-otel-status-error "gptel-otel-core")

(defconst gptel-otel--agent-seams
  '((gptel--map-tool-args (tool-spec args) gptel-otel--around-map-tool-args :around)
    (gptel-agent--task (main-cb agent-type description prompt)
                        gptel-otel--around-agent-task :around)))

(defun gptel-otel--agent-adapter-status (&optional base)
  "Return observational optional-agent compatibility facts."
  (let ((seams (mapcar #'gptel-otel--seam-status gptel-otel--agent-seams))
        (base (or base (gptel-otel--base-adapter-capabilities))))
    (list :available (featurep 'gptel-agent)
          :supported (and (featurep 'gptel-agent)
                          (plist-get base 'base)
                          (plist-get (plist-get base 'tool) :supported)
                          (cl-every (lambda (status) (plist-get status :supported)) seams))
          :seams seams)))

(defun gptel-otel--agent-adapter-uninstall ()
  (dolist (pair gptel-otel--agent-installed-advices)
    (advice-remove (car pair) (cdr pair)))
  (dolist (seam gptel-otel--agent-seams)
    (pcase-let ((`(,symbol ,_args ,advice ,_where) seam))
      (when (advice-member-p advice symbol) (advice-remove symbol advice))))
  (setq gptel-otel--agent-installed-advices nil))

(defun gptel-otel--agent-adapter-install (&optional base)
  "Reconcile agent advice only with active compatible base and tool layers."
  (gptel-otel--agent-adapter-uninstall)
  (let ((status (gptel-otel--agent-adapter-status base)))
    (setq gptel-otel--agent-adapter-capability status)
    (cond ((not (plist-get status :available)) nil)
          ((not (plist-get status :supported))
           (when (cl-find-if-not (lambda (seam) (plist-get seam :supported))
                                 (plist-get status :seams))
             (gptel-otel--warn-unsupported-layer "Optional gptel-agent" status)))
          ((gptel-otel--base-adapter-active-p t)
           (dolist (seam gptel-otel--agent-seams)
               (pcase-let ((`(,symbol ,_args ,advice ,where) seam))
                 (advice-add symbol where advice)
                 (push (cons symbol advice) gptel-otel--agent-installed-advices)))))
    status))

(defun gptel-otel--agent-adapter-on-load ()
  (when (bound-and-true-p gptel-otel-mode)
    (let ((base (gptel-otel--base-adapter-install)))
      (gptel-otel--agent-adapter-install base)
      (gptel-otel--mode-reconcile-prompt base))))

(defun gptel-otel--around-map-tool-args (orig tool-spec args)
  (let ((value (funcall orig tool-spec args)))
    (condition-case nil
        (when (equal (gptel-tool-name tool-spec) "Agent")
          (setq gptel-otel--next-agent-execution (gethash args gptel-otel--agent-bindings)))
      (error nil))
    value))

(defun gptel-otel--agent-callback (span context original value)
  (let ((returned (funcall original value)))
    (condition-case err
        (progn
          (gptel-otel--set-semantic-attributes span :kind 'observation-finish :type "agent" :output value)
          (gptel-otel-trace-end-span (gptel-otel--context-trace context) span
                                     (if (and (stringp value) (string-prefix-p "Error:" value))
                                         (gptel-otel-status-error value) (gptel-otel-status-ok)))
          (gptel-otel--maybe-export (gptel-otel--context-trace context)))
      (error (display-warning 'gptel-otel (format "Agent finish failed: %s" err) :warning)))
    returned))

(defun gptel-otel--around-agent-task (orig main-cb agent-type description prompt)
  "Preserve ORIG's result/errors while containing every telemetry operation."
  (let* ((execution (prog1 gptel-otel--next-agent-execution
                      (setq gptel-otel--next-agent-execution nil)))
         (parent-fsm (or (car-safe execution) gptel-otel--parent-fsm))
         (parent-context (condition-case nil
                             (and parent-fsm (gptel-otel--context parent-fsm))
                           (error nil)))
         ;; A subagent is part of its invoking request, not a fresh buffer
         ;; decision.  In particular, a suppressed parent must not create a
         ;; trace merely because the child runs in a different buffer.
         (decision (condition-case nil
                       (and parent-fsm
                            (or (gptel-otel--request-decision-for-fsm parent-fsm)
                                (and parent-context :trace)))
                     (error nil)))
         (tool-span (condition-case nil (nth 2 execution) (error nil)))
         span wrapped child)
    (condition-case nil
        (when (and parent-context tool-span)
          (setq span
                (gptel-otel-trace-start-span
                 (gptel-otel--context-trace parent-context) (format "invoke_agent %s" agent-type) tool-span
                 (append (gptel-otel--observation-attributes
                          parent-context "agent" (list :type agent-type :description description :prompt prompt) nil)
                         (delq nil (list (gptel-otel--string-attr "langfuse.observation.metadata.subagent_type" agent-type)
                                         (gptel-otel--string-attr "langfuse.observation.metadata.description" description)))))))
      (error (setq span nil)))
    (setq wrapped (if span (apply-partially #'gptel-otel--agent-callback span parent-context main-cb) main-cb))
    (let ((gptel-otel--pending-agent span)
          ;; gptel-agent starts its child synchronously, but bind this as well
          ;; for any prompt transforms it invokes before returning.
          (gptel-otel--request-decision decision))
      (condition-case upstream
          (setq child (funcall orig wrapped agent-type description prompt))
        (error
         (when span (condition-case nil
                        (progn (gptel-otel-trace-end-span (gptel-otel--context-trace parent-context) span
                                                         (gptel-otel-status-error (error-message-string upstream)))
                               (gptel-otel--maybe-export (gptel-otel--context-trace parent-context)))
                      (error nil)))
         (signal (car upstream) (cdr upstream)))))
    (when (and decision child)
      ;; Child requests use their own transform list, so carry the immutable
      ;; parent decision explicitly instead of consulting child buffer state.
      (condition-case nil
          (unless (memq (gptel-fsm-state child) '(DONE ERRS ABRT))
            (puthash child decision gptel-otel--request-decisions))
        (error nil)))
    (when (and (eq decision :trace) span child)
      (condition-case nil
          (unless (gptel-otel--context child) (gptel-otel--new-context child parent-context span))
        (error nil)))
    child))

(with-eval-after-load 'gptel-agent (gptel-otel--agent-adapter-on-load))

(provide 'gptel-otel-agent-adapter)
;;; gptel-otel-agent-adapter.el ends here
